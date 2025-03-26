from flask import Flask, request, jsonify, send_from_directory
from flask_cors import CORS
import time
import random
import threading
import re

app = Flask(__name__, static_folder="static")
CORS(app)

games = {}

# --- Randomness Settings ---
MEDIUM_RANDOM_CHANCE = 0.10
HARD_RANDOM_CHANCE = 0.02

# --- Core Logic ---
def check_winner(board, n_to_win):
    rows, cols = len(board), len(board[0])
    for r in range(rows):
        for c in range(cols):
            if board[r][c] == "": continue
            player = board[r][c]
            if c + n_to_win <= cols and all(board[r][c+i] == player for i in range(n_to_win)):
                return player, [(r, c+i) for i in range(n_to_win)]
            if r + n_to_win <= rows and all(board[r+i][c] == player for i in range(n_to_win)):
                return player, [(r+i, c) for i in range(n_to_win)]
            if r + n_to_win <= rows and c + n_to_win <= cols and all(board[r+i][c+i] == player for i in range(n_to_win)):
                return player, [(r+i, c+i) for i in range(n_to_win)]
            if r - n_to_win + 1 >= 0 and c + n_to_win <= cols and all(board[r-i][c+i] == player for i in range(n_to_win)):
                return player, [(r-i, c+i) for i in range(n_to_win)]
    if all(cell != "" for row in board for cell in row):
        return "draw", []
    return None, []

def get_empty_cells(board):
    return [(r, c) for r in range(len(board)) for c in range(len(board[0])) if board[r][c] == ""]

def get_active_zone(board, radius=3):
    active = set()
    for r in range(len(board)):
        for c in range(len(board[0])):
            if board[r][c] != "":
                for dr in range(-radius, radius + 1):
                    for dc in range(-radius, radius + 1):
                        nr, nc = r + dr, c + dc
                        if 0 <= nr < len(board) and 0 <= nc < len(board[0]) and board[nr][nc] == "":
                            active.add((nr, nc))
    return list(active)

def get_lines(board, r, c):
    directions = [(1,0), (0,1), (1,1), (-1,1)]
    lines = []
    for dr, dc in directions:
        line = ""
        for i in range(-5, 6):
            nr, nc = r + dr * i, c + dc * i
            if 0 <= nr < len(board) and 0 <= nc < len(board[0]):
                line += board[nr][nc] if board[nr][nc] else "."
            else:
                line += "#"
        lines.append(line)
    return lines

def detect_patterns(board, r, c, player, opponent, n_to_win):
    lines = get_lines(board, r, c)
    patterns = {
        f"{player*4}.": 10000,
        f".{player*4}": 10000,
        f"{player*3}.{player}": 8000,
        f"{player}.{player*3}": 8000,
        f"{player*2}.{player*2}": 9000,
        f"{opponent*4}.": 9500,
        f".{opponent*4}": 9500
    }
    score = 0
    for line in lines:
        for pat, val in patterns.items():
            if pat in line:
                score = max(score, val)
    return score

def evaluate_position(board, r, c, player, n_to_win):
    directions = [(1,0), (0,1), (1,1), (-1,1)]
    score = 0
    for dr, dc in directions:
        count, blocks = 1, 0
        for i in range(1, n_to_win):
            nr, nc = r + dr*i, c + dc*i
            if 0 <= nr < len(board) and 0 <= nc < len(board[0]):
                if board[nr][nc] == player: count += 1
                elif board[nr][nc] != "": blocks += 1; break
                else: break
        for i in range(1, n_to_win):
            nr, nc = r - dr*i, c - dc*i
            if 0 <= nr < len(board) and 0 <= nc < len(board[0]):
                if board[nr][nc] == player: count += 1
                elif board[nr][nc] != "": blocks += 1; break
                else: break
        if blocks < 2:
            score = max(score, 10 ** count)
    return score

def detect_critical_threat(board, player, n_to_win):
    empty = get_empty_cells(board)
    for r, c in empty:
        board[r][c] = player
        result, _ = check_winner(board, n_to_win)
        if result == player:
            board[r][c] = ""
            return (r, c)
        board[r][c] = ""
    return None

# --- Game State & Moves ---
def apply_bot_move(game_id, r, c):
    game = games[game_id]
    game["board"][r][c] = game["turn"]
    winner, winline = check_winner(game["board"], game["settings"]["n_to_win"])
    if winner:
        game["winner"] = winner
        game["winline"] = winline
    else:
        game["turn"] = "O" if game["turn"] == "X" else "X"
        if game["mode"] == "bvb":
            threading.Thread(target=lambda: make_bot_move(game_id)).start()

def make_bot_move(game_id):
    game = games[game_id]
    delay = game["settings"]["bot_delay"] / 1000
    difficulty = game["settings"]["bot_difficulty"]

    def bot_logic():
        time.sleep(delay)
        board = game["board"]
        turn = game["turn"]
        opponent = "O" if turn == "X" else "X"
        n_to_win = game["settings"]["n_to_win"]
        empty = get_active_zone(board, radius=4)
        if not empty:
            empty = get_empty_cells(board)

        move = None

        if difficulty == "very easy":
            move = random.choice(empty)

        elif difficulty == "easy":
            move = detect_critical_threat(board, turn, n_to_win) or detect_critical_threat(board, opponent, n_to_win)
            if not move:
                move = random.choice(empty)

        elif difficulty == "medium":
            if random.random() < MEDIUM_RANDOM_CHANCE:
                move = random.choice(empty)
            else:
                move = detect_critical_threat(board, turn, n_to_win) or detect_critical_threat(board, opponent, n_to_win)
                if not move:
                    best_score = -float('inf')
                    for r, c in empty:
                        self_score = evaluate_position(board, r, c, turn, n_to_win)
                        opp_score = evaluate_position(board, r, c, opponent, n_to_win)
                        score = self_score * 2 + opp_score
                        if score > best_score:
                            best_score = score
                            move = (r, c)

        elif difficulty == "hard":
            if random.random() < HARD_RANDOM_CHANCE:
                move = random.choice(empty)
            else:
                move = detect_critical_threat(board, turn, n_to_win) or detect_critical_threat(board, opponent, n_to_win)
                if not move:
                    best_score = -float('inf')
                    for r, c in empty:
                        pattern_score = detect_patterns(board, r, c, turn, opponent, n_to_win)
                        heuristic_score = evaluate_position(board, r, c, turn, n_to_win)
                        score = pattern_score * 100 + heuristic_score
                        if score > best_score:
                            best_score = score
                            move = (r, c)

        if move:
            apply_bot_move(game_id, move[0], move[1])

    threading.Thread(target=bot_logic).start()

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
    game = games[data["game_id"]]
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
            threading.Thread(target=lambda: make_bot_move(data["game_id"])).start()
    return jsonify({"board": game["board"], "turn": game["turn"], "winner": game["winner"], "winline": game.get("winline", [])})

@app.route("/state", methods=["GET"])
def state():
    game_id = request.args.get("game_id")
    game = games[game_id]
    return jsonify({
        "board": game["board"],
        "turn": game["turn"],
        "winner": game["winner"],
        "winline": game.get("winline", [])
    })

if __name__ == "__main__":
    app.run(debug=True)