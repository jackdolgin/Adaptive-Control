// create a random subject code
//set participant values		
function getRandomString(length, chars) {
    var result = '';
    for (var i = length; i > 0; --i) result += chars[Math.round(Math.random() * (chars.length - 1))];
    return result;
}

const Sub_Code = getRandomString(8, '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ');

sessionStorage.setItem("sscode", Sub_Code);


// submits data to database using php code
function submit_data() {
    let xhr = new XMLHttpRequest();
    xhr.open('POST', 'php/server-side.php'); // change 'write_data.php' to point to php script.
    xhr.setRequestHeader('Content-Type', 'application/json');
    console.log('humm');
    xhr.onload = function() {
        if(xhr.status == 200){
            console.log("xhr.responseText");
            console.log(xhr.responseText);
            const response = JSON.parse(xhr.responseText);
            console.log(response.success);
        }
    };
    console.log('hummer');
    xhr.send('data=' + JSON.stringify(trialArray, null, '\t') + '&table=' + sqlTable);
}

// // submits data to database using php code
// function submit_data() {
//         let formHTML = [
//             "<form id='sendtoPHP' method='post' action='server-side.php' style='display: none'>",
//                 "<input type='hidden' name='put-studyid-here' id = 'put-studyid-here' value = ''/>",
//                 "<input type='hidden' name='put-sscode-here' id = 'put-sscode-here' value = ''/>",
//                 "<input type='hidden' name='put-data-here' id = 'put-data-here' value = ''/>",
//             "</form>"
//             ].join("\n")

//     document.querySelector("body").innerHTML += formHTML


//     // collect each task dataset
//     let data = trialArray;

//     // convert array of objects to string
//     data = array_to_text(data)


//     // submit data
//     document.getElementById('put-studyid-here').value = sub_code;
//     document.getElementById('put-sscode-here').value = "demo-exp";
//     document.getElementById('put-data-here').value = data;
//     document.getElementById('sendtoPHP').submit();
// }

// // converts an array of objects to csv formatted text
// function array_to_text(args) {
//     let result, ctr, keys, columnDelimiter, lineDelimiter, data;

//     data = args || null;
//     if (data == null || !data.length) {
//         return null;
//     }

//     columnDelimiter = args.columnDelimiter || ',';
//     lineDelimiter = args.lineDelimiter || '\n';

//     keys = Object.keys(data[0]);

//     result = '';
//     result += keys.join(columnDelimiter);
//     result += lineDelimiter;

//     data.forEach(function (item) {
//         ctr = 0;
//         keys.forEach(function (key) {
//             if (ctr > 0) result += columnDelimiter;

//             result += '"' + item[key] + '"';
//             ctr++;
//         });
//         result += lineDelimiter;
//     });

//     return result;
// }