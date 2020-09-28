import React, { Component } from "react";
const MyContext = React.createContext();

class MyProvider extends Component {
  constructor(props) {
    super(props);
    this.state = {
      name: "Ting",
      age: 100,
      cool: true,
    };
  }
  render() {
    return (
      <MyContext.Provider
        value={{
          state: this.state,
          growAYearOlder: () =>
            this.setState({
              age: this.state.age + 1,
            }),
        }}
      >
        {this.props.children}
      </MyContext.Provider>
    );
  }
}

const Family = (props) => {
  return (
    <div className="family">
      <Person />
    </div>
  );
};

class Person extends Component {
  render() {
    return (
      <div className="person">
        <MyContext.Consumer>
          {(context) => {
            return (
              <React.Fragment>
                <p>Age: I'm inside the {context.state.age}</p>
                <p>Name: I'm inside the {context.state.name}</p>
                <button onClick={context.growAYearOlder}>🍰</button>
              </React.Fragment>
            );
          }}
        </MyContext.Consumer>
      </div>
    );
  }
}

class App extends Component {
  render() {
    return (
      <MyProvider>
        <div className="App">
          <p>I'm the app</p>
          <Family />
        </div>
      </MyProvider>
    );
  }
}

export default App;
