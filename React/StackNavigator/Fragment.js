import React, { Component } from "react";
import { AppRegistry, Button, StyleSheet, Image, Text, View } from "react-native";

import { StackNavigator } from "react-navigation";

import Reactnav from "./Components/ReactNav";
import ReactSecNav from "./Components/ReactSecNav";
import ReactThirdNav from "./Components/ReactThiNav";

const App = StackNavigator({
  Home: { screen: Reactnav },
  NavSec: { screen: ReactSecNav },
  NavThird: { screen: ReactThirdNav },
});

AppRegistry.registerComponent("react_github", () => App);
