from flask import Flask, request, jsonify, send_from_directory
from flask_cors import CORS
import time
import threading
from pyswip import Prolog

app = Flask(__name__, static_folder="static")
CORS(app)

# Initialize Prolog and load the logic file
prolog = Prolog()
prolog.consult("game_logic.pl")

games = {}

def convert_board_to_prolog(board):
    """Convert Python board ("" for empty, "X"/"O") into Prolog list-of-lists using atoms: empty, x, o."""
    prolog_board = []
    for row in board:
        prolog_row = []
        for cell in row:
            if cell == "":
                prolog_row.append("empty")
            elif cell == "X":
                prolog_row.append("x")
            elif cell == "O":
                prolog_row.append("o")
        prolog_board.append(prolog_row)
    return prolog_board

def convert_board_from_prolog(board):
    """Convert board from Prolog (empty, x, o) back to Python format."""
    py_board = []
    for row in board:
        py_row = []
        for cell in row:
            if cell == "empty":
                py_row.append("")
            elif cell == "x":
                py_row.append("X")
            elif cell == "o":
                py_row.append("O")
        py_board.append(py_row)
    return py_board

def check_winner(board, n_to_win):
    prolog_board = convert_board_to_prolog(board)
    query = f'check_winner({prolog_board}, {n_to_win}, Winner, WinLine).'
    result = list(prolog.query(query, maxresult=1))
    if result:
        winner = result[0]["Winner"]
        if isinstance(winner, bytes):
            winner = winner.decode("utf-8")
        winline = result[0]["WinLine"]
        return winner, winline
    return None, []

def apply_bot_move(game_id, r, c):
    game = games[game_id]
    game["board"][r][c] = game["turn"]
    winner, winline = check_winner(game["board"], game["settings"]["n_to_win"])
    if winner:
        game["winner"] = winner
        game["winline"] = winline
    else:
        # Switch turn and (for bot-vs-bot mode) schedule next bot move.
        game["turn"] = "O" if game["turn"] == "X" else "X"
        if game["mode"] == "bvb":
            threading.Thread(target=lambda: make_bot_move(game_id)).start()

def make_bot_move(game_id):
    game = games[game_id]
    delay = game["settings"]["bot_delay"] / 1000.0
    difficulty = game["settings"]["bot_difficulty"]
    time.sleep(delay)
    board = game["board"]
    turn = game["turn"]
    n_to_win = game["settings"]["n_to_win"]
    prolog_board = convert_board_to_prolog(board)
    turn_atom = "x" if turn == "X" else "o"
    query = f'bot_move({prolog_board}, {turn_atom}, "{difficulty}", {n_to_win}, Move).'
    print("Bot move query:", query)
    result = list(prolog.query(query, maxresult=1))
    print("Bot move result:", result)
    if result:
        # Expecting a list [Row,Col] now
        move = result[0]["Move"]
        try:
            r_move, c_move = move  # move should be a list/tuple of two integers
        except Exception as e:
            print("Error unpacking move:", e)
            return
        apply_bot_move(game_id, r_move, c_move)

@app.route("/")
def index():
    return send_from_directory("static", "index.html")

@app.route("/<path:path>")
def static_proxy(path):
    return send_from_directory("static", path)

@app.route("/start", methods=["POST"])
def start_game():
    data = request.json
    board = [["" for _ in range(data["cols"])] for _ in range(data["rows"])]
    game_id = str(len(games) + 1)
    game = {
        "id": game_id,
        "board": board,
        "turn": "X",
        "winner": None,
        "winline": [],
        "mode": data["mode"],
        "settings": {
            "bot_delay": data["bot_delay"],
            "bot_difficulty": data["bot_difficulty"],
            "n_to_win": data["n_to_win"]
        }
    }
    games[game_id] = game
    if data["mode"] == "bvb":
        threading.Thread(target=lambda: make_bot_move(game_id)).start()
    return jsonify({"game_id": game_id, "board": board, "turn": "X"})

@app.route("/move", methods=["POST"])
def move():
    data = request.json
    game_id = data["game_id"]
    if game_id not in games:
        return jsonify({"error": "Game not found"}), 404
    game = games[game_id]
    r, c = data["row"], data["col"]
    if game["board"][r][c] != "" or game["winner"]:
        return jsonify({"error": "Invalid move"}), 400
    game["board"][r][c] = game["turn"]
    winner, winline = check_winner(game["board"], game["settings"]["n_to_win"])
    if winner:
        game["winner"] = winner
        game["winline"] = winline
    else:
        game["turn"] = "O" if game["turn"] == "X" else "X"
        if game["mode"] == "pve" and game["turn"] == "O":
            threading.Thread(target=lambda: make_bot_move(game_id)).start()
    return jsonify({"board": game["board"], "turn": game["turn"], "winner": game["winner"], "winline": game.get("winline", [])})

@app.route("/state", methods=["GET"])
def state():
    game_id = request.args.get("game_id")
    if game_id not in games:
        return jsonify({"error": "Game not found"}), 404
    game = games[game_id]
    return jsonify({
        "board": game["board"],
        "turn": game["turn"],
        "winner": game["winner"],
        "winline": game.get("winline", [])
    })

if __name__ == "__main__":
    app.run(debug=True)
