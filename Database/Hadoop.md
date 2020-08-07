hadoop namenode -format

jps

# hadoop/bin

> start-all.sh stop-all.sh

hadoop fs -ls / hadoop fs -put XXX /input hadoop fs -get XXX XXX hadoop js -rmr input

- 5_1 example

  javac -classpath
  /Users/ztlevi/Developer/hadoop-1.2.1/hadoop-core-1.2.1.jar:/Users/ztlevi/Developer/hadoop-1.2.1/lib/commons-cli-1.2.jar
  -d word_count_class/ WordCount.java

  cd word_count_class

  jar -cvf wordcount.jar \*.class

  hadoop jar wordcount.jar WordCount input_wordcount output_wordcount
