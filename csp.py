from enum import Enum
import sys
import json
from queue import Queue


class Agenda:
    def __init__(self):
        self.queue = Queue()
        self.hash_set = set()

    def add(self, item):
        if item not in self.hash_set:
            self.hash_set.add(item)
            self.queue.put(item)

    def pop(self):
        item = self.queue.get()
        self.hash_set.remove(item)
        return item

    def empty(self):
        return self.queue.empty()


def err_exit(*args, **kwargs):
    print("[ERROR] ", file=sys.stderr, end="")
    print(*args, file=sys.stderr, **kwargs)
    exit(1)


class cmp(Enum):
    EQ = 0
    NEQ = 1
    G = 2
    L = 3


class Cell:
    curr = None
    vals = []

    def __init__(self, vals):
        self.vals = vals


class Constraint:
    name = "unnamed"
    comp = None
    arg1 = None
    arg2 = None

    def get_inv(self):
        comp = self.comp
        if comp in [cmp.G, comp.L]:
            comp = [cmp.G, cmp.L][comp == cmp.G]
        return Constraint("inv_" + self.name, comp, self.arg2, self.arg1)

    def __init__(self, name, comp, arg1, arg2):
        self.name = name
        self.comp = comp
        self.arg1 = arg1
        self.arg2 = arg2


class Network:
    def __init__(self, clean=False):
        if not clean:
            self.cells = {}
            self.constraints = []
            self.all_solutions = False

        self.solutions = None
        self.check_over = False

    def build_cell(self, name, vals):
        self.cells[name] = Cell(vals)

    def build_constraint(self, name, comp, arg1, arg2):
        if comp not in cmp:
            err_exit(f"unknown comparator enum: {comp}")
        self.constraints.append(Constraint(name, comp, arg1, arg2))

    def optimize(self):
        # Build arcs and agenda
        agenda = Agenda()
        arcs = {}
        for x in self.constraints:
            # Skip if constraint isn't for two cells
            if x.arg1 not in self.cells or x.arg2 not in self.cells:
                continue

            inv = x.get_inv()
            agenda.add(x)
            agenda.add(inv)
            arcs[x.arg2] = x
            arcs[inv.arg2] = inv

        # Optimize Loop
        while not agenda.empty():
            cons = agenda.pop()

            left_cell = self.cells[cons.arg1]
            right_cell = self.cells[cons.arg2]
            for l in left_cell.vals:
                remove_left = True
                for r in right_cell.vals:
                    if self.eval_expr(cons.name, cons.comp, l, r):
                        remove_left = False
                        break
                if remove_left:
                    left_cell.vals.remove(l)
                    agenda.add(cons.get_inv())

        # Check if it is contradictory after optimization
        for x in self.cells:
            if not self.cells[x].vals:
                return False

        return True

    def run(self):
        if self.is_check_over():
            return

        if self.is_solution():
            self.save_solution()
            return

        for x in self.cells:
            cell = self.cells[x]
            if cell.curr is None:
                for val in cell.vals:
                    cell.curr = val
                    if self.assert_constrains():
                        self.run()
                cell.curr = None

    def run_csp(self, all=False):
        self.__init__(clean=True)
        self.all_solutions = all

        if self.optimize():
            self.run()

        return self.solutions

    def is_solution(self):
        for x in self.cells:
            if self.cells[x].curr is None:
                return False
        return True

    def is_check_over(self):
        if self.check_over:
            return True
        else:
            self.check_over = (not self.all_solutions) and self.solutions is not None
            return self.check_over

    def save_solution(self):
        solution = []
        for x in self.cells:
            solution.append((x, self.cells[x].curr))

        if self.all_solutions:
            if self.solutions is None:
                self.solutions = []
            if solution not in self.solutions:
                self.solutions.append(solution)
        else:
            self.solutions = solution

    @staticmethod
    def eval_expr(name, comp, a, b):
        if comp == cmp.EQ:
            return a == b
        elif comp == cmp.NEQ:
            return a != b
        elif comp == cmp.L:
            return a < b
        elif comp == cmp.G:
            return a > b
        else:
            err_exit(f"wrong comparator for constraint {name}")

    def eval(self, name, comp, arg1, arg2):
        if arg1 not in self.cells and arg2 not in self.cells:
            err_exit(f"both '{arg1}' and '{arg2}' are undeclared for constraint '{name}'")

        if arg1 not in self.cells or arg2 not in self.cells:
            val = [arg2, arg1][arg1 not in self.cells]
            cell = [arg1, arg2][arg1 not in self.cells]

            if self.cells[cell].curr is None:
                return True
            a, b = self.cells[cell].curr, val
        else:
            a, b = self.cells[arg1].curr, self.cells[arg2].curr
            if a is None or b is None:
                return True

        return self.eval_expr(name, comp, a, b)

    def assert_constrains(self):
        for cons in self.constraints:
            if not self.eval(cons.name, cons.comp, cons.arg1, cons.arg2):
                return False
        return True


def test_marx():
    marx = Network()
    marx.build_cell("pianista", ["grucho", "harpo", "chico"])
    marx.build_cell("harfiarz", ["grucho", "harpo", "chico"])
    marx.build_cell("gadula", ["grucho", "harpo", "chico"])
    marx.build_cell("pieniadze", ["grucho", "harpo", "chico"])
    marx.build_cell("hazard", ["grucho", "harpo", "chico"])
    marx.build_cell("zwierzeta", ["grucho", "harpo", "chico"])

    # 1) Pianista, harfiarz i gadula to różne osoby.
    marx.build_constraint("pianista/harfiarz", cmp.NEQ, "pianista", "harfiarz")
    marx.build_constraint("harfiarz/gadula", cmp.NEQ, "harfiarz", "gadula")
    marx.build_constraint("pianista/gadula", cmp.NEQ, "pianista", "gadula")

    # 2) Ten kto lubi pieniądze nie jest tym, kto lubi hazard, a ten z kolei nie lubi zwierząt
    marx.build_constraint("pieniadze/hazard", cmp.NEQ, "pieniadze", "hazard")
    marx.build_constraint("hazard/zwierzeta", cmp.NEQ, "hazard", "zwierzeta")

    # 3) Gaduła nie lubi hazardu.
    marx.build_constraint("gadula/hazard", cmp.NEQ, "gadula", "hazard")

    # 4) Harfiarz lubi zwierzęta.
    marx.build_constraint("harfiarz==zwierzeta", cmp.EQ, "harfiarz", "zwierzeta")

    # 5) Groucho nie lubi zwierząt.
    marx.build_constraint("grucho!=zwierzeta", cmp.NEQ, "grucho", "zwierzeta")

    # 6) Harpo nigdy nic nie mówi.
    marx.build_constraint("harpo!=gadula", cmp.NEQ, "harpo", "gadula")

    # 7) Chico gra na pianinie.
    marx.build_constraint("chico==pianista", cmp.EQ, "chico", "pianista")

    print(json.dumps(marx.run_csp(), indent=4))
    print(json.dumps(marx.run_csp(all=True), indent=4))


def test_nums():
    nums = Network()

    nums.build_cell("A", [1, 2, 3])
    nums.build_cell("B", [1, 2, 3])
    nums.build_cell("C", [1, 2, 3])

    nums.build_constraint("A>B", cmp.G, "A", "B")
    nums.build_constraint("B=C", cmp.EQ, "B", "C")

    print(json.dumps(nums.run_csp(all=True), indent=4))


if __name__ == "__main__":
    test_marx()
    test_nums()
