https://reactnavigation.org/docs/navigators/navigation-actions#Reset
---

    let resetAction = NavigationActions.reset({
      index: 0,
      actions: [
        NavigationActions.navigate({
          routeName: 'homePage',
          params: { selectedTab: jumpToTab },
        }),
      ],
    });
    this.props.navigation.dispatch(resetAction);
