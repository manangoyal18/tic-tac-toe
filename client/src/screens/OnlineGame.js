import React, { useEffect, useState, useRef } from 'react';
import { View, Text, Alert, StyleSheet, ImageBackground } from 'react-native';
import bg from '../../assets/bg.jpeg';
import Cell from '../components/Cell';
import { io } from 'socket.io-client'; // Or use a WebSocket fallback

const emptyMap = [["", "", ""], ["", "", ""], ["", "", ""]];

export default function OnlineGame() {
  const [map, setMap] = useState(emptyMap);
  const [currentTurn, setCurrentTurn] = useState("x");
  const [playerSymbol, setPlayerSymbol] = useState(null); // "x" or "o"
  const socketRef = useRef(null);

  useEffect(() => {
    // Connect to Erlang backend (assuming WebSocket server on port 8080)
    //socketRef.current = new WebSocket('ws://<YOUR_SERVER_IP>:8080');
    socketRef.current = new WebSocket('ws://localhost:8080');


    socketRef.current.onopen = () => {
      console.log('Connected to server');
    };

    socketRef.current.onmessage = (message) => {
      const data = JSON.parse(message.data);

      if (data.type === 'assign_symbol') {
        setPlayerSymbol(data.symbol);
      } else if (data.type === 'game_state') {
        setMap(data.board);
        setCurrentTurn(data.turn);
      } else if (data.type === 'game_result') {
        Alert.alert("Game Over", data.result, [
          { text: "OK", onPress: resetGame }
        ]);
      }
    };

    socketRef.current.onerror = (err) => {
      console.error("Socket error:", err.message);
    };

    socketRef.current.onclose = () => {
      console.log("Connection closed");
    };

    return () => {
      socketRef.current.close();
    };
  }, []);

  const resetGame = () => {
    setMap(emptyMap);
    setCurrentTurn("x");
    socketRef.current.send(JSON.stringify({ type: 'reset' }));
  };

  const onPress = (rowIndex, columnIndex) => {
    if (map[rowIndex][columnIndex] !== "") return;
    if (currentTurn !== playerSymbol) return;

    const payload = {
      type: "move",
      row: rowIndex,
      col: columnIndex,
      symbol: playerSymbol,
    };

    socketRef.current.send(JSON.stringify(payload));
  };

  return (
    <View style={styles.container}>
      <ImageBackground source={bg} style={styles.bg} resizeMode="contain">
        <Text style={styles.turnText}>You are: {playerSymbol?.toUpperCase()}</Text>
        <Text style={styles.turnText2}>Current Turn: {currentTurn.toUpperCase()}</Text>
        <View style={styles.map}>
          {map.map((row, rIdx) => (
            <View key={rIdx} style={styles.row}>
              {row.map((cell, cIdx) => (
                <Cell key={cIdx} cell={cell} onPress={() => onPress(rIdx, cIdx)} />
              ))}
            </View>
          ))}
        </View>
      </ImageBackground>
    </View>
  );
}

const styles = StyleSheet.create({
  container: { flex: 1, backgroundColor: "#242D34" },
  bg: { flex: 1, alignItems: "center", justifyContent: "center", paddingTop: 35 },
  turnText: { fontSize: 24, color: "white", position: "absolute", top: 150  },
  turnText2: { fontSize: 24, color: "white", position: "absolute", top: 50  },
  map: { width: "80%", aspectRatio: 1 },
  row: { flex: 1, flexDirection: "row" },
});
