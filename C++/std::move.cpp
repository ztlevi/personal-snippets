#include <cmath>
#include <iostream>
#include <memory>
#include <vector>

typedef std::pair<double, double> Point;
typedef std::vector<Point> Points;

class Trajectory {
public:
  Trajectory(Points &&points) : points_(std::move(points)) {}
  double ComputeLength() {
    double total_length = 0;
    for (int i = 0; i < points_.size() - 1; i++) {
      double delta_x = points_[i + 1].first - points_[i].first;
      double delta_y = points_[i + 1].second - points_[i].second;
      total_length += sqrt(delta_x * delta_x + delta_y * delta_y);
    }
    total_length_ = total_length;
    return total_length_;
  }

private:
  Points points_;
  double total_length_;
};

int main(int argc, char *argv[]) {
  Points points;

  points.emplace_back(std::make_pair(0, 0));
  points.emplace_back(std::make_pair(1, 0));
  points.emplace_back(std::make_pair(2, 0));
  points.emplace_back(std::make_pair(3, 0));

  std::cout << "Before points moved: " << points.size() << std::endl;

  Trajectory trajectory = Trajectory(std::move(points));
  std::cout << "ComputeLength: " << trajectory.ComputeLength() << std::endl;

  // Points used after it's moved
  std::cout << "After points moved: " << points.size() << std::endl;
  return 0;
}
