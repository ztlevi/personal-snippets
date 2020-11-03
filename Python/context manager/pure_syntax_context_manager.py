from sqlite3 import connect
 
class temptable:
  def __init__(self, cur):
    self.cur = cur
  def __enter__(self):
    self.cur.execute('create table points(x int, y int)')
  def __exit__(self, *args):
    self.cur.execute('drop table points')
    
with connect('test.db') as conn:
  cur = conn.cursor()
  with temptable(cur):
    cur.execute('insert into points (x, y) values(1,1)')
    cur.execute('insert into points (x, y) values(2,1)')
    cur.execute('insert into points (x, y) values(1,2)')
    for row in cur.execute('select x, y from points'):
      print(row)
    for row in cur.execute('select sum(x * y) from points'):
      print(row)