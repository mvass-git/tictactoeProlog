const socket = io();
let state;

function render() {
    const boardEl = document.getElementById('board');
    boardEl.style.gridTemplate = `repeat(${state.rows}, 1fr) / repeat(${state.cols}, 1fr)`;
    boardEl.innerHTML = '';
    state.board.forEach((row, r) => row.forEach((cell, c) => {
        const div = document.createElement('div');
        div.className = 'cell'; div.textContent = cell;
        div.onclick = () => socket.emit('move', { r, c });
        boardEl.appendChild(div);
    }));
    document.getElementById('status').innerText = state.over ? (state.winner ? `${state.winner} wins!` : 'Draw!') : `${state.current}'s turn`;
}

socket.on('update', data => { state = data; render(); });

document.getElementById('new').onclick = () => {
    const cfg = ['rows','cols','win_len','mode','difficulty','delay'].reduce((o,id) => (o[id]=document.getElementById(id).value, o), {});
    socket.emit('new_game', cfg);
};