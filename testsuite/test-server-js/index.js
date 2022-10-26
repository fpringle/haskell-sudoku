const fetch = require('node-fetch');

const sugoku_url = 'https://sugoku2.herokuapp.com/'
const my_url = 'http://localhost:3421/'

const encodeBoard = (board) => board.reduce((result, row, i) => result + `%5B${encodeURIComponent(row)}%5D${i === board.length -1 ? '' : '%2C'}`, '')
const encodeParams = (params) => Object.keys(params).map(key => key + '=' + `%5B${encodeBoard(params[key])}%5D`).join('&');

function print_board(board) {
  const rows = board.map(row => {
    row = row.map(x => x > 0 ? (''+x) : ' ');
    return [...row.slice(0, 3), '|', ...row.slice(3, 6), '|', ...row.slice(6, 9)].join(' ');
  });
  rows.splice(6, 0, '------+------+------');
  rows.splice(3, 0, '------+------+------');
  console.log(rows.join('\n'));
}

function get_puzzle(difficulty) {
  const board_url = sugoku_url + 'board?difficulty=' + difficulty;
  return fetch(board_url).then(response => response.json()).catch(console.warn);
}

function get_sugoku_solution(board) {
  const start_time = Date.now();
  const sol_url = sugoku_url + 'solve';

  return fetch(sol_url, {
    method: 'POST',
    body: encodeParams(board),
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
  }).then(response => {
    const time_taken = Date.now() - start_time;
    return response.json().then(json => [json.solution, time_taken]);
  }).catch(console.warn);
}

function get_my_solution(board) {
  const start_time = Date.now();
  const my_sol_url = my_url + 'solve';
  return fetch(my_sol_url, {
    method: 'POST',
    body: JSON.stringify(board),
  }).then(response => {
    const time_taken = Date.now() - start_time;
    return response.json().then(json => [json.board, time_taken]);
  }).catch(console.warn);
}

console.log('Fetching a "hard" sudoku puzzle from sugoku2.herokuapp.com');

get_puzzle('hard').then(board => {
  console.log('Puzzle:');
  print_board(board.board);
  console.log('\nGetting solutions\n');

  return Promise.all([
    get_sugoku_solution(board),
    get_my_solution(board),
  ]);
}).then(([[sugoku_sol, sugoku_time], [my_sol, my_time]]) => {
  console.log(`Solution from sugoku2.herokuapp.com (${sugoku_time} ms):`);
  print_board(sugoku_sol);

  console.log(`\nMy solution (${my_time} ms):`);
  print_board(my_sol);
  console.log();

  const ss = sol => sol.map(row => row.join('')).join('');

  if (ss(sugoku_sol) === ss(my_sol)) console.log('Same solution!');
  else console.log('Different solution...');
});
