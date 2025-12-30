#!/usr/bin/env node
// Validates Vega-Lite specs using the official vega-lite compiler.
// Usage: echo '{"data":{"values":[]},"mark":"point"}' | node validate-vega-lite.mjs
// Output: JSON with {valid: boolean, errors?: [{message, ...}]}

import * as vl from 'vega-lite';
import * as readline from 'readline';

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false
});

let input = '';

rl.on('line', (line) => {
  input += line;
});

rl.on('close', () => {
  try {
    const spec = JSON.parse(input);
    const warnings = [];

    // Custom logger to capture warnings
    const logger = {
      level: () => 0,
      warn: (msg) => warnings.push({message: msg, type: 'warning'}),
      info: () => {},
      debug: () => {}
    };

    try {
      vl.compile(spec, {logger});

      if (warnings.length > 0) {
        console.log(JSON.stringify({valid: true, warnings}));
      } else {
        console.log(JSON.stringify({valid: true}));
      }
    } catch (compileError) {
      console.log(JSON.stringify({
        valid: false,
        errors: [{
          message: compileError.message,
          type: 'error'
        }]
      }));
    }
  } catch (parseError) {
    console.log(JSON.stringify({
      valid: false,
      errors: [{
        message: `Invalid JSON: ${parseError.message}`,
        type: 'parse-error'
      }]
    }));
  }
});
