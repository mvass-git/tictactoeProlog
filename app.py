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
    """Convert Python board ("" for empty, "X"/"O") into Prolog list-of-lists
    using atoms: empty, x, o."""
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

def convert_winline(winline):
    """
    Convert winline (list of winning positions returned from Prolog)
    into a list of lists of integers.
    
    Expected winline format: each position is returned as a list of characters 
    representing a term like [',','(','3',',',' ','1',')'].
    This function joins the characters into a string, removes punctuation, 
    splits on whitespace, and converts the resulting parts into integers.
    
    Example:
      Input: [[',','(','3',',',' ','1',')'], [',','(','3',',',' ','2',')']]
      Output: [[3, 1], [3, 2]]
    """
    if isinstance(winline, list):
        new_winline = []
        for pos in winline:
            # If pos is a list of characters, join them into a string.
            if isinstance(pos, list):
                s = ''.join(pos)
            else:
                s = str(pos)
            # Remove any leading/trailing whitespace.
            s = s.strip()
            # Remove punctuation characters: comma, '(', ')'
            for ch in [',', '(', ')']:
                s = s.replace(ch, '')
            # Now split the string by whitespace.
            parts = s.split()
            try:
                numbers = [int(x) for x in parts]
                new_winline.append(numbers)
            except Exception as e:
                print("Error converting winline position:", e)
                new_winline.append(s)
        print("Converted winline:", new_winline)
        return new_winline
    print("winline is not a list:", winline)
    return winline

def check_winner(board, n_to_win):
    prolog_board = convert_board_to_prolog(board)
    query = f'check_winner({prolog_board}, {n_to_win}, Winner, WinLine).'
    result = list(prolog.query(query, maxresult=1))
    if result:
        winner = result[0]["Winner"]
        if isinstance(winner, bytes):
            winner = winner.decode("utf-8")
        winline = result[0]["WinLine"]
        winline = convert_winline(winline)
        return winner, winline
    return None, []

def apply_bot_move(game_id, r, c):
    game = games[game_id]
    # For pvp and bvb modes use the current symbol, for pve bot uses its own symbol.
    if game["mode"] in ["pvp", "bvb"]:
        game["board"][r][c] = game["turn"]
    else:
        game["board"][r][c] = game["bot_symbol"]
    winner, winline = check_winner(game["board"], game["settings"]["n_to_win"])
    if winner:
        game["winner"] = winner
        game["winline"] = winline
    else:
        # Change turn:
        if game["mode"] in ["pvp", "bvb"]:
            game["turn"] = "O" if game["turn"] == "X" else "X"
        else:
            game["turn"] = game["player_symbol"] if game["turn"] == game["bot_symbol"] else game["bot_symbol"]
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
        move = result[0]["Move"]  # Expected to be a list [Row, Col]
        try:
            r_move, c_move = move
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
    # For pve mode, player selects a symbol; for pvp/bvb, default to X for first turn.
    if data["mode"] in ["pvp", "bvb"]:
        player_symbol = "X"
        bot_symbol = "O"
    else:
        player_symbol = data.get("player_symbol", "X")
        bot_symbol = "O" if player_symbol == "X" else "X"
    game = {
        "id": game_id,
        "board": board,
        "turn": "X",  # default: "X" starts
        "winner": None,
        "winline": [],
        "mode": data["mode"],
        "player_symbol": player_symbol,
        "bot_symbol": bot_symbol,
        "settings": {
            "bot_delay": data["bot_delay"],
            "bot_difficulty": data["bot_difficulty"],
            "n_to_win": data["n_to_win"]
        }
    }
    games[game_id] = game
    # For pve, if initial turn is bot's, trigger bot move
    if game["mode"] == "pve" and game["turn"] == game["bot_symbol"]:
        threading.Thread(target=lambda: make_bot_move(game_id)).start()
    if game["mode"] == "bvb":
        threading.Thread(target=lambda: make_bot_move(game_id)).start()
    return jsonify({"game_id": game_id, "board": board, "turn": game["turn"]})

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
    if game["mode"] == "pve":
        game["board"][r][c] = game["player_symbol"]
    else:
        game["board"][r][c] = game["turn"]
    winner, winline = check_winner(game["board"], game["settings"]["n_to_win"])
    if winner:
        game["winner"] = winner
        game["winline"] = winline
    else:
        if game["mode"] == "pve":
            game["turn"] = game["bot_symbol"] if game["turn"] == game["player_symbol"] else game["player_symbol"]
            if game["turn"] == game["bot_symbol"]:
                threading.Thread(target=lambda: make_bot_move(game_id)).start()
        else:
            game["turn"] = "O" if game["turn"] == "X" else "X"
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
