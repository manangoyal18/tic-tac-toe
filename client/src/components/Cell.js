import React from 'react';
import { TouchableOpacity, Text, StyleSheet } from 'react-native';

export default function Cell({ value, onPress }) {
  return (
    <TouchableOpacity 
      style={styles.cell} 
      onPress={onPress}
      disabled={value !== ''}
    >
      <Text style={[styles.text, value === 'X' ? styles.x : styles.o]}>
        {value}
      </Text>
    </TouchableOpacity>
  );
}

const styles = StyleSheet.create({
  cell: {
    flex: 1,
    aspectRatio: 1,
    backgroundColor: '#191F24',
    margin: 5,
    justifyContent: 'center',
    alignItems: 'center',
    borderRadius: 5,
  },
  text: {
    fontSize: 48,
    fontWeight: 'bold',
  },
  x: {
    color: '#3498DB',
  },
  o: {
    color: '#E74C3C',
  },
});