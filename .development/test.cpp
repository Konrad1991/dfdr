#include <functional>
#include <iostream>
#include <vector>

class Node {
public:
  std::string name;
  double value;
  double deriv;

  Node(std::string name, double value) : name(name), value(value), deriv(0.0) {}
};

// similar to node_line
class NodeLine : public Node {
public:
  std::vector<Node *> connected_nodes;
  std::function<double(const std::vector<Node *> &)> value_fct;
  std::function<std::vector<double>(double, const std::vector<Node *> &)>
      deriv_fct;

  NodeLine(
      std::string name, std::vector<Node *> connected_nodes,
      std::function<double(const std::vector<Node *> &)> value_fct,
      std::function<std::vector<double>(double, const std::vector<Node *> &)>
          deriv_fct)
      : Node(name, 0.0), connected_nodes(connected_nodes), value_fct(value_fct),
        deriv_fct(deriv_fct) {}

  void forward() { value = value_fct(connected_nodes); }

  void backward() {
    std::vector<double> grads = deriv_fct(deriv, connected_nodes);
    for (size_t i = 0; i < connected_nodes.size(); ++i) {
      connected_nodes[i]->deriv += grads[i];
    }
  }
};

int main() {
  Node a("a", 2.0);
  Node b("b", 3.0);
  auto value_fct = [](const std::vector<Node *> &inputs) {
    return inputs[0]->value - inputs[1]->value;
  };
  auto deriv_fct = [](double grad, const std::vector<Node *> &inputs) {
    return std::vector<double>{grad, -grad}; // da = 1, db = -1
  };
  NodeLine line("line", {&a, &b}, value_fct, deriv_fct);
  line.forward();
  std::cout << "Forward value: " << line.value << std::endl;
  line.deriv = 1.0;
  line.backward();
  std::cout << "Gradient of a: " << a.deriv << std::endl; // Should be 1.0
  std::cout << "Gradient of b: " << b.deriv << std::endl; // Should be -1.0

  return 0;
}
