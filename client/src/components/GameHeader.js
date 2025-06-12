import React from 'react';
import { View, Text, StyleSheet, TouchableOpacity } from 'react-native';

export default function GameHeader({ title, onMenuPress }) {
  return (
    <View style={styles.header}>
      <TouchableOpacity onPress={onMenuPress}>
        <Text style={styles.menuButton}>←</Text>
      </TouchableOpacity>
      <Text style={styles.title}>{title}</Text>
      <View style={{ width: 30 }} /> {/* Spacer for alignment */}
    </View>
  );
}

const styles = StyleSheet.create({
  header: {
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center',
    marginBottom: 20,
  },
  title: {
    fontSize: 24,
    fontWeight: 'bold',
    color: '#FFFFFF',
  },
  menuButton: {
    fontSize: 24,
    color: '#FFFFFF',
    padding: 10,
  },
});