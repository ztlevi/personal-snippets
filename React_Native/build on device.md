rm -rf ios/build/; kill \$(lsof -t -i:8081); react-native run-ios

react-native run-ios --configuration release --device
