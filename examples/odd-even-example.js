require('shelljs/global');

const args = require('..').run(`
Naval Fate.
Usage:
  foo APFEL [options]
  foo BIRNE [options]
  foo MANDARINE [options]
  foo BANANE [options] EBERHARD
options:
  -a
  -b
  -c
  -d
  -e
  -f
  -g
  -h
  -i
  -j
  -k
  -l
  -m
  -n
  -o
  -p
  -q
  -r
  -s
  -t
  -u
  -v
  -w
  -x
  -y
  -z
IGORE:
`, { laxPlacement: true });

echo(args);
