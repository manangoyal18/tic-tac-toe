import React from 'react';
import { ImageBackground, StyleSheet } from 'react-native';
import { NavigationContainer } from '@react-navigation/native';
import { createStackNavigator } from '@react-navigation/stack';

import HomeScreen from './src/screens/HomeScreen';
import SinglePlayerGame from './src/screens/SinglePlayerGame';
import MultiPlayerGame from './src/screens/MultiPlayerGame';

import bg from './assets/bg.jpeg'; 

const Stack = createStackNavigator();

export default function App() {
  return (
    <ImageBackground source={bg} style={styles.background} resizeMode="cover">
      <NavigationContainer>
        <Stack.Navigator
          initialRouteName="Home"
          screenOptions={{
            headerShown: false,
          }}
        >
          <Stack.Screen name="Home" component={HomeScreen} />
          <Stack.Screen name="SinglePlayer" component={SinglePlayerGame} />
          <Stack.Screen name="MultiPlayer" component={MultiPlayerGame} />
        </Stack.Navigator>
      </NavigationContainer>
    </ImageBackground>
  );
}

const styles = StyleSheet.create({
  background: {
    flex: 1,
    width: '100%',
    height: '100%',
  },
});
