const fs = require('fs')
const path = require('path')

// Ensure directory
const benchDir = fs.statSync('benchmarks')
if (!benchDir || !benchDir.isDirectory()) {
  console.error('Missing benchmarks/ directory');
  process.exit(1)
}

// Prepare benchmarks/elm-package.json
const rootJson = JSON.parse(fs.readFileSync('elm-package.json'))

const newSourceDirs =
  rootJson['source-directories']
    .map(dir => path.join('..', dir))
    .concat('.')

const benchJson = Object.assign(rootJson, {
  'summary': 'Benchmark for the parent project',
  'dependencies': Object.assign(rootJson.dependencies, {
    'BrianHicks/elm-benchmark': '2.0.0 <= v < 3.0.0'
  }),
  'exposed-modules': [],
  'source-directories': newSourceDirs
})

fs.writeFileSync(path.join('benchmarks', 'elm-package.json'), JSON.stringify(benchJson, null, 4))
