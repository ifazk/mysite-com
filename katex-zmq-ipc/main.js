const katex = require("katex");
const zmq = require("zeromq");

async function run() {
    const sock = new zmq.Reply;

    await sock.bind("ipc:///tmp/katex");

    for await (const [msg] of sock) {
        let msgObj = JSON.parse(msg);
        let latex = msgObj.latex;
        let options = msgObj.options;
        options.throwOnError = false;
        let html = katex.renderToString(latex, options);
        console.log(`Recieved\n${msg}`);
        console.log(`Sending\n${html}`);
        await sock.send(html);
    }
}

run();
