const readline = require('readline');

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
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
function insert(mes, oldTree) {
  if (Object.keys(oldTree).length === 0) {
    return { Node: mes, Tree: oldTree }
  } else {
    if (oldTree.Node.LogMessage.time > mes.LogMessage.time) {
      return { Node: mes, Tree: oldTree }
    } else {
      return { Node: oldTree.Node, Tree: insert(mes, oldTree.Tree) }
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
    return [tree.Node].concat(inOrder(tree.Tree))
  }
}

// whatWentWrong:: [LogMessage] -> [String]
function whatWentWrong(logs) {
  return inOrder(build(logs.filter(l => l.LogMessage.type === "Error" && l.LogMessage.severity >= 50))).map(m => m.LogMessage.message)
}

function measureTime(data) {
  const date = Date.now();
  console.log("test whatWentWrong - \n", JSON.stringify(whatWentWrong(data), null, 2));
  console.log(Date.now() - date);
}

readInput(measureTime);


// tests
// const str = "I 11 Initiating self-destruct sequence\nE 70 3 Way too many pickles\nE 65 8 Bad pickle- flange interaction detected\nW 5 Flange is due for a check- up"
// const tree = { 'Node': { 'LogMessage': { 'type': 'Info', 'time': 11, 'message': 'Initiating' } }, 'Tree': { "Node": { "LogMessage": { "type": 'Error', "severity": 70, "time": 22, "message": 'Way' } }, "Tree": {} } };
// const mes = { LogMessage: { type: 'Warning', time: 111, message: 'Flange' } };
// const list = [{ LogMessage: { type: 'Info', time: 11, message: 'Initiating' } },
// { LogMessage: { type: 'Error', severity: 70, time: 13, message: 'Way' } },
// { LogMessage: { type: 'Error', severity: 65, time: 8, message: 'Bad' } },
// { LogMessage: { type: 'Error', severity: 35, time: 8, message: 'foo' } },
// { LogMessage: { type: 'Warning', time: 5, message: 'Flange' } }]

// console.log("test parseMessage - ",parseMessage("E 20 2 Too many pickles"))
// console.log("test parseMessage - ",parseMessage("I 7 Out for lunch, back in two time steps"))
// console.log("test parseMessage - ",parseMessage("W 5 Flange is due for a check-up"))
// console.log("test parseMessage - ",parseMessage("Xc 9 Back from lunch"))

// console.log("test parse - \n", parse(str))
// console.log("test insert - \n", JSON.stringify(insert(mes, tree), null, 2))
// console.log("test build - \n", JSON.stringify(build(list), null, 2))
// console.log("test inOrder - \n", JSON.stringify(inOrder(tree), null, 2))
// console.log("test whatWentWrong - \n", JSON.stringify(whatWentWrong(list), null, 2))
