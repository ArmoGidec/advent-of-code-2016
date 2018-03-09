let md5 = require("md5");

function compute1(id) {
    let password = '';
    for (let index = 0; password.length < 8; index++) {
        let hash = md5(id + index);
        if (hash.slice(0, 5) === "00000") {
            password += hash[5];
            console.log(password);
        }
    }
    return password;
}

function compute2(id) {
    let password = ['_', '_', '_', '_', '_', '_', '_', '_'];
    for (let index = 0; password.includes('_'); index++) {
        let hash = md5(id + index);
        if (hash.slice(0, 5) === "00000") {
            let i = hash[5];
            if (i>=0 && i<8 && password[i] === '_') {
                password[i] = hash[6];
                console.log(password);    
            }
        }
    }
    return password.join('');
}

function Program() {
    // let part1 = compute1("reyedfim");
    let part2 = compute2("reyedfim");

    console.clear();
    console.log({
        // part1,
        part2
    });
    return;
}

Program();
