async function readConfig() {
  try {
    let content = await readFile("config.json");
    let obj = JSON.parse(content.toString());
    console.log(obj);
  } catch (error) {
    console.error("An error occurred", error);
  }
}
