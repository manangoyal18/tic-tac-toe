import React, { useState, useEffect } from 'react';
import { View, StyleSheet, Alert } from 'react-native';
import GameHeader from '../components/GameHeader';
import Cell from '../components/Cell';

const emptyBoard = [
  ['', '', ''],
  ['', '', ''],
  ['', '', '']
];

export default function SinglePlayerGame({ navigation }) {
  const [board, setBoard] = useState(emptyBoard);
  const [currentPlayer, setCurrentPlayer] = useState('X');
  const [gameOver, setGameOver] = useState(false);

  useEffect(() => {
    // Call server to create single player game session
    createSinglePlayerGame();
  }, []);

  useEffect(() => {
    if (currentPlayer === 'O' && !gameOver) {
      makeBotMove();
    }
  }, [currentPlayer, gameOver]);

  const createSinglePlayerGame = async () => {
    try {
      const response = await fetch('http://your-erlang-server:8080/api/games', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ type: 'single_player' }),
      });
      const data = await response.json();
      console.log('Single-player game started:', data);
      // If the server sends a game_id or config, you can store it if needed
    } catch (error) {
      console.error('Error creating single player game:', error);
    }
  };

  const handlePress = (row, col) => {
    if (board[row][col] !== '' || currentPlayer !== 'X' || gameOver) return;

    const newBoard = [...board];
    newBoard[row][col] = 'X';
    setBoard(newBoard);

    checkGameResult(newBoard, 'X');
    setCurrentPlayer('O');
  };

  const makeBotMove = () => {
    const availableMoves = [];
    board.forEach((row, rowIndex) => {
      row.forEach((cell, colIndex) => {
        if (cell === '') {
          availableMoves.push({ row: rowIndex, col: colIndex });
        }
      });
    });

    if (availableMoves.length === 0) return;

    // Try to win
    for (const move of availableMoves) {
      const testBoard = JSON.parse(JSON.stringify(board));
      testBoard[move.row][move.col] = 'O';
      if (checkWinner(testBoard)) {
        updateBoard(move.row, move.col, 'O');
        return;
      }
    }

    // Block player
    for (const move of availableMoves) {
      const testBoard = JSON.parse(JSON.stringify(board));
      testBoard[move.row][move.col] = 'X';
      if (checkWinner(testBoard)) {
        updateBoard(move.row, move.col, 'O');
        return;
      }
    }

    // Random move
    const randomMove = availableMoves[Math.floor(Math.random() * availableMoves.length)];
    updateBoard(randomMove.row, randomMove.col, 'O');
  };

  const updateBoard = (row, col, player) => {
    const newBoard = [...board];
    newBoard[row][col] = player;
    setBoard(newBoard);
    checkGameResult(newBoard, player);
    setCurrentPlayer('X');
  };

  const checkGameResult = (currentBoard, player) => {
    if (checkWinner(currentBoard)) {
      Alert.alert('Game Over', `Player ${player} wins!`, [
        { text: 'OK', onPress: resetGame }
      ]);
      setGameOver(true);
    } else if (checkTie(currentBoard)) {
      Alert.alert('Game Over', "It's a tie!", [
        { text: 'OK', onPress: resetGame }
      ]);
      setGameOver(true);
    }
  };

  const checkWinner = (currentBoard) => {
    for (let i = 0; i < 3; i++) {
      if (
        currentBoard[i][0] !== '' &&
        currentBoard[i][0] === currentBoard[i][1] &&
        currentBoard[i][1] === currentBoard[i][2]
      ) {
        return true;
      }
    }

    for (let j = 0; j < 3; j++) {
      if (
        currentBoard[0][j] !== '' &&
        currentBoard[0][j] === currentBoard[1][j] &&
        currentBoard[1][j] === currentBoard[2][j]
      ) {
        return true;
      }
    }

    if (
      currentBoard[0][0] !== '' &&
      currentBoard[0][0] === currentBoard[1][1] &&
      currentBoard[1][1] === currentBoard[2][2]
    ) {
      return true;
    }

    if (
      currentBoard[0][2] !== '' &&
      currentBoard[0][2] === currentBoard[1][1] &&
      currentBoard[1][1] === currentBoard[2][0]
    ) {
      return true;
    }

    return false;
  };

  const checkTie = (currentBoard) => {
    return currentBoard.every(row => row.every(cell => cell !== ''));
  };

  const resetGame = () => {
    setBoard(emptyBoard);
    setCurrentPlayer('X');
    setGameOver(false);
  };

  return (
    <View style={styles.container}>
      <GameHeader 
        title="Single Player" 
        onMenuPress={() => navigation.goBack()} 
      />
      
      <View style={styles.board}>
        {board.map((row, rowIndex) => (
          <View key={`row-${rowIndex}`} style={styles.row}>
            {row.map((cell, colIndex) => (
              <Cell
                key={`cell-${rowIndex}-${colIndex}`}
                value={cell}
                onPress={() => handlePress(rowIndex, colIndex)}
              />
            ))}
          </View>
        ))}
      </View>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#242D34',
    padding: 20,
  },
  board: {
    flex: 1,
    aspectRatio: 1,
    marginTop: 50,
  },
  row: {
    flex: 1,
    flexDirection: 'row',
  },
});
