// submits data to database using php code
function submitData(data, table, nextPage) {
    console.log(data);
    if (table === mainSQLTable) sessionStorage.setItem("sscode", Sub_Code);
    
    let xhr = new XMLHttpRequest();
    xhr.open('POST', "php/server-side.php");
    xhr.setRequestHeader("Content-Type", "application/json");

    xhr.onload = function() {
        if(xhr.status == 200){
            window.open(nextPage + ".html?" + window.location.search.slice(1) , "_self");
        }
    };

    xhr.send("data=" + JSON.stringify(data, null, "\t") + "&table=" + table);
}

function createSubmissionArray(x, y, z, b, c) {
    document.querySelector("#" + x).addEventListener("click", function() {
        
        let submissionArray = {Sub_Code: sessionStorage.getItem("sscode")};
        
        for (const el of document.getElementsByTagName(y)){
            if (x==="dem-submit") {
                let optionsIndex = el.options.selectedIndex;
                if (optionsIndex === 0) {
                    document.getElementById("plz-answer").style.cssText += "display: inline-block; visibility: visible;";
                    return;
                } else{
                    submissionArray[el.id] = el.options[optionsIndex].value;
                }
            } else {
                submissionArray[el.id] = el.value;
            }
        }
        
        submissionArray[c] = document.getElementById(c).value;
        if (x==="submit-form") sessionStorage.setItem("form_submit", 1);
        
        submitData([submissionArray,], z, b);
    });
}