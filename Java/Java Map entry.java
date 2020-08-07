for (Map.Entry<String, String> entry : map.entrySet())
{
    System.out.println(entry.getKey() + "/" + entry.getValue());
}

for (String key : mp.keySet()){
    //iterate over key
    System.out.println(key+" "+mp.get(key));
}