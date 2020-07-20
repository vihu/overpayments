## Test overpayments
import math
import random
import hypothesis.strategies as s

from dataclasses import dataclass
from hypothesis import given
from string import ascii_lowercase

@dataclass
class Node:
    ''' Class for tracking a node'''
    name: str
    value: int

    def __hash__(self):
        # We only hash the name for uniqueness
        return hash(self.name)

    def __eq__(self, other):
        # If the name of the node is same, we consider equal, ignoring the value
        return self.name == other.name

def answer(input_list, target):
    # sum the node values
    delta = sum([node.value for node in input_list]) - target

    # reverse sort by node value
    reverse_sorted = sorted(input_list, key=lambda node: node.value, reverse=True)

    # copy (don't overwrite reverse_sorted list)
    ans = reverse_sorted

    # straight up subtract delta from top guy
    ans[0] = Node(ans[0].name, ans[0].value - delta)

    for i in range(0, len(ans) - 1):
        # do we have any overage?
        second = ans[i+1]
        delta_forward = second.value - ans[i].value

        if delta_forward >= 0:
            # subtract overage from second max
            ans[i+1] = Node(second.name, second.value - delta_forward)
            # split forward and construct new list
            head = list(map(lambda x: Node(x.name,
                                           x.value + math.floor(delta_forward / (i+2))),
                            ans[:i+2]))
            tail = ans[i+2:]
            ans = head + tail
        else:
            break

    # return sorted by node name

    # it is possible that we may have undercut the output
    # as in the case of all equal values in the input_list
    # run a final check, distributing the leftover
    temp_ans = final_check(ans, target)

    final_ans = sorted(temp_ans, key=lambda x: x.name)

    return final_ans

def final_check(temp_ans, target):
    temp_sum0 = sum([node.value for node in temp_ans])
    leftover = target - temp_sum0

    # in place shuffle the list
    random.shuffle(temp_ans)
    index = 0
    while leftover != 0:
        node = temp_ans[index]
        temp_ans[index] = Node(node.name, node.value + 1)
        index += 1
        leftover -= 1
        # redo, if we reach the end
        if index == len(temp_ans):
            index = 0

    return temp_ans

def test_1():
    input_list = [Node("a", 20), Node("b", 110), Node("c", 110), Node("d", 110), Node("e", 30)]
    assert answer(input_list, 200) == [Node("a", 20), Node("b", 50), Node("c", 50), Node("d", 50), Node("e", 30)]

def test_2():
    input_list = [Node("a", 20), Node("b", 90), Node("c", 110), Node("d", 30)]
    assert answer(input_list, 200) == [Node("a", 20), Node("b", 75), Node("c", 75), Node("d", 30)]

# ===================================================================
# Property testing
# ===================================================================

# s.text() -> generate some random name (doesn't actually matter), non-empty
# s.integers() -> generate integers between 1 and 10000
NodeStrategy = s.builds(Node, s.text(ascii_lowercase, min_size=1), s.integers(min_value=1, max_value=10000))

# generate sets of nodes (to ensure that we have unique nodes)
@given(s.sets(NodeStrategy, min_size=3))
def test_3(nodes):
    node_list = list(nodes)
    print(f"nodes: {node_list}")

    # check that we have some nodes
    assert len(nodes) >= 1

    # check that we generated uniquely named nodes
    node_names = [node.name for node in node_list]
    assert sorted(list(set(node_names))) == sorted(node_names)

    # set some random target such that sum(node_values) <= target
    node_values = [node.value for node in node_list]
    sum_node_values = sum(node_values)
    target = random.randint(1, sum_node_values)
    print(f"sum: {sum_node_values}, target: {target}")

    ans = answer(node_list, target)
    print(f"ans: {ans}")
    new_values = [node.value for node in ans]
    assert sum(new_values) == target
