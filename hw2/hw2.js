const readline = require('readline');

const rl = readline.createInterface({
  input: process.stdin
})

function readInput(callback) {
  let data = [];
  rl.on('line', (line) => {
    data.push(parseMessage(line));
  });

  rl.on('close', () => {
    callback(data);
  });
}




// parseMessage:: String -> LogMessage
function parseMessage(str) {
  const words = str.split(" ");
  switch (words[0]) {
    case "I":
      return { LogMessage: { type: 'Info', time: parseInt(words[1]), message: words.slice(2, words.length).join(" ") } }
    case "W":
      return { LogMessage: { type: 'Warning', time: parseInt(words[1]), message: words.slice(2, words.length).join(" ") } }
    case "E":
      return { LogMessage: { type: 'Error', severity: parseInt(words[1]), time: parseInt(words[2]), message: words.slice(3, words.length).join(" ") } }
    default:
      return { Unknown: str }
  }
}

// parse:: String -> [LogMessage]
function parse(text) {
  return text.split(/\r?\n/).map(str => parseMessage(str))
}

// insert:: LogMessage -> MessageTree -> MessageTree
function insert(mes, tree) {
  if (mes.hasOwnProperty('Unknown')) {
    return tree
  }
  if (Object.keys(tree).length === 0) {
    return { Node: mes, before: {}, after: {} }
  } else {
    if (tree.Node.LogMessage.time > mes.LogMessage.time) {
      return Object.assign({}, tree, { before: insert(mes, tree.before) })
    } else {
      return Object.assign({}, tree, { after: insert(mes, tree.after) })
    }
  }
}

// build:: [LogMessage] -> MessageTree
function build(logs) {
  if (logs.length === 0) {
    return {}
  } else {
    return insert(logs[0], build(logs.slice(1, logs.length)))
  }
}

// inOrder:: MessageTree -> [LogMessage]
function inOrder(tree) {
  if (Object.keys(tree).length === 0) {
    return []
  } else {
    return inOrder(tree.before).concat([tree.Node].concat(inOrder(tree.after)))
  }
}

// whatWentWrong:: [LogMessage] -> [String]
function whatWentWrong(logs) {
  return inOrder(build(logs.filter(l => l.LogMessage.type === "Error" && l.LogMessage.severity >= 50))).map(m => m.LogMessage.message)
}


readInput(data => console.log(JSON.stringify(whatWentWrong(data), null, 2)));
