#include <iostream>
#include <unordered_set>
class Student {
  int mId;
  std::string mName;

public:
  Student(int id, std::string name) : mId(id), mName(name) {}
  void displayInfo() { std::cout << mId << " :: " << mName << std::endl; }
  bool operator==(const Student &obj) const {
    if (mId == obj.mId)
      return true;
    else
      return false;
  }
  int getId() const { return mId; }
  std::string getName() const { return mName; }
};
namespace std {
template <> struct hash<Student> {
  size_t operator()(const Student &obj) const {
    return hash<int>()(obj.getId());
  }
};
} // namespace std
// Custom Hash Functor that will compute the hash on the
// passed string objects length
struct StudentHasher {
  size_t operator()(const Student &obj) const {
    return std::hash<std::string>()(obj.getName());
  }
};
// Custom comparator that compares the string objects by length
struct StudentComparator {
  bool operator()(const Student &obj1, const Student &obj2) const {
    if (obj1.getName() == obj2.getName())
      return true;
    return false;
  }
};
int main() {
  // Declaring unordered_set of Student
  std::unordered_set<Student> setOfStudents;
  std::cout << "Creating an Unordered_set by unique ID\n";
  // Inserting elements
  setOfStudents.insert(Student(11, "John"));
  setOfStudents.insert(Student(12, "Harry"));
  setOfStudents.insert(Student(13, "Ritz"));
  setOfStudents.insert(Student(14, "John"));
  // Trying to insert with duplicate ID
  auto res = setOfStudents.insert(Student(12, "Varun"));
  if (res.second == false)
    std::cout << "Failed to insert with ID 12" << std::endl;
  for (Student st : setOfStudents)
    st.displayInfo();
  /*-----------------------------------------------*/
  // Declaring unordered_set of Student
  std::unordered_set<Student, StudentHasher, StudentComparator> setOfStudByName;
  std::cout << "Creating an Unordered_set by Unique Name\n";
  // Inserting elements
  setOfStudByName.insert(Student(11, "John"));
  setOfStudByName.insert(Student(12, "Harry"));
  setOfStudByName.insert(Student(13, "Ritz"));
  res = setOfStudByName.insert(Student(14, "John"));
  if (res.second == false)
    std::cout << "Failed to insert with duplicate name - John" << std::endl;
  // Trying to insert with duplicate ID
  res = setOfStudByName.insert(Student(12, "Varun"));
  if (res.second == false)
    std::cout << "Failed to insert with ID 12" << std::endl;
  else
    std::cout << "Successful in inserting with ID 12 again" << std::endl;
  for (Student st : setOfStudByName)
    st.displayInfo();
  return 0;
}
