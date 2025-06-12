import React, { useState, useEffect, useRef } from 'react';
import { View, StyleSheet, Alert, Text } from 'react-native';
import GameHeader from '../components/GameHeader';
import Cell from '../components/Cell';
import Button from '../components/Button';

const emptyBoard = [
  ['', '', ''],
  ['', '', ''],
  ['', '', '']
];

export default function MultiPlayerGame({ navigation }) {
  const [board, setBoard] = useState(emptyBoard);
  const [currentPlayer, setCurrentPlayer] = useState('X'); // Local player's symbol
  const [myTurn, setMyTurn] = useState(true); // This will switch after each valid move
  const [scores, setScores] = useState({ X: 0, O: 0, ties: 0 });
  const [gameOver, setGameOver] = useState(false);
  const ws = useRef(null);

  useEffect(() => {
    createMultiPlayerGame();

    return () => {
      if (ws.current) {
        ws.current.close();
      }
    };
  }, []);

  const createMultiPlayerGame = async () => {
    try {
      const response = await fetch('http://your-erlang-server:8080/api/games', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ type: 'multi_player' }),
      });
      const data = await response.json();
      const gameId = data.game_id;

      ws.current = new WebSocket(`ws://your-erlang-server:8080/ws/games/${gameId}`);

      ws.current.onopen = () => {
        console.log('WebSocket connected');
      };

      ws.current.onmessage = (event) => {
        const message = JSON.parse(event.data);

        if (message.type === 'move') {
          const { row, col, player } = message;
          const updatedBoard = [...board];
          updatedBoard[row][col] = player;
          setBoard(updatedBoard);
          setCurrentPlayer(player === 'X' ? 'O' : 'X');
          setMyTurn(true);
          checkGameResult(updatedBoard, player);
        }
      };

      ws.current.onerror = (error) => {
        console.error('WebSocket error:', error);
      };

      ws.current.onclose = () => {
        console.log('WebSocket closed');
      };
    } catch (error) {
      console.error('Failed to create multiplayer game:', error);
    }
  };

  const handlePress = (row, col) => {
    if (!myTurn || board[row][col] !== '' || gameOver) return;

    const newBoard = [...board];
    newBoard[row][col] = currentPlayer;
    setBoard(newBoard);

    // Send move to opponent
    if (ws.current && ws.current.readyState === WebSocket.OPEN) {
      ws.current.send(
        JSON.stringify({
          type: 'move',
          row,
          col,
          player: currentPlayer
        })
      );
    }

    setMyTurn(false);
    checkGameResult(newBoard, currentPlayer);
  };

  const checkGameResult = (currentBoard, player) => {
    if (checkWinner(currentBoard)) {
      const newScores = { ...scores };
      newScores[player] += 1;
      setScores(newScores);
      Alert.alert('Game Over', `Player ${player} wins!`, [
        { text: 'OK', onPress: () => setGameOver(true) }
      ]);
    } else if (checkTie(currentBoard)) {
      const newScores = { ...scores };
      newScores.ties += 1;
      setScores(newScores);
      Alert.alert('Game Over', "It's a tie!", [
        { text: 'OK', onPress: () => setGameOver(true) }
      ]);
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
    setMyTurn(true);
    setGameOver(false);
  };

  return (
    <View style={styles.container}>
      <GameHeader title="Multi Player" onMenuPress={() => navigation.goBack()} />

      <View style={styles.scoreContainer}>
        <View style={styles.scoreBox}>
          <Text style={styles.scoreLabel}>Player X</Text>
          <Text style={styles.scoreValue}>{scores.X}</Text>
        </View>
        <View style={styles.scoreBox}>
          <Text style={styles.scoreLabel}>Ties</Text>
          <Text style={styles.scoreValue}>{scores.ties}</Text>
        </View>
        <View style={styles.scoreBox}>
          <Text style={styles.scoreLabel}>Player O</Text>
          <Text style={styles.scoreValue}>{scores.O}</Text>
        </View>
      </View>

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

      <View style={styles.buttonContainer}>
        <Button title="MENU" onPress={() => navigation.goBack()} style={styles.menuButton} />
        <Button title="NEW GAME" onPress={resetGame} />
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
  scoreContainer: {
    flexDirection: 'row',
    justifyContent: 'space-between',
    marginBottom: 20,
  },
  scoreBox: {
    alignItems: 'center',
    backgroundColor: '#191F24',
    padding: 10,
    borderRadius: 5,
    flex: 1,
    marginHorizontal: 5,
  },
  scoreLabel: {
    color: '#FFFFFF',
    fontSize: 16,
  },
  scoreValue: {
    color: '#FFFFFF',
    fontSize: 24,
    fontWeight: 'bold',
  },
  board: {
    flex: 1,
    aspectRatio: 1,
  },
  row: {
    flex: 1,
    flexDirection: 'row',
  },
  buttonContainer: {
    flexDirection: 'row',
    justifyContent: 'space-between',
    marginTop: 20,
  },
  menuButton: {
    backgroundColor: '#E74C3C',
  },
});
