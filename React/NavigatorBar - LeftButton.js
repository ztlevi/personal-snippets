  renderButton(image) {
    return <TouchableOpacity
              onPress={()=>{
                 this.props.navigation.goBack();
             }}
           >
      <Image style={{width:22, height:22, margin:5}} source={image}></Image>
    </TouchableOpacity>
  }

      <View style={styles.container}>
        <NavigatorBar
          title={"Girl"}
          style={{
            backgroundColor: '#EE6363'
          }}
          leftButton={
            this.renderButton(require('./res/images/ic_arrow_back_white_36pt.png'))
          }
          rightButton={
            this.renderButton(require('./res/images/ic_star.png'))
          }
        />
      </View>