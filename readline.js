import * as fs from "fs";

function _readline() {
    const buffer = Buffer.alloc(1 << 13);
    const n = fs.readSync(0, buffer);
    return buffer.toString("utf8", 0, n);
}
