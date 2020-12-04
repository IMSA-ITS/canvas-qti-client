from string import Template

with open("index.html.tmpl") as t:
    template = t.read()

with open("env/auth") as f:
    params = f.read()

s = Template(template)
print(s.substitute(params=params))
