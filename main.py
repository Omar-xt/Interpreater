import dis, tokenize, ast

src = \
"""
a = 10

def foo(num):
    print("i get from you: ", num)
print(foo(10))
"""

dis.dis(src)



print("---------Token----------")
with tokenize.open("src.py") as f:
    tokens = tokenize.generate_tokens(f.readline)
    for token in tokens:
        print(token)

print(ast.dump(ast.parse(src), indent=2))

print(10, 20)
