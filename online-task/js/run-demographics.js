(async () => {
    const languageCodes = await (async () => {
        let phpFetched = await fetch('php/grab_languages.php');
        phpFetched = await phpFetched.text();
        let htmlDoc = (new DOMParser()).parseFromString(phpFetched, "text/html");
     
        htmlDoc = Array.from(htmlDoc
            .querySelector('#lang-table-container')
            .querySelectorAll('table tr'))
        .slice(1)
        .map(el => Array.from(el.querySelectorAll('td')))
        .filter(children => children[2].innerHTML === 'Default' &&
                            children[1].innerHTML.split('-')[0] === 'en')
        .map(children => {
            return {
                code: children[1].innerHTML,
                description: (/\(([^)]+)\)/).exec(children[0].innerHTML)[1],
            };
        })
        .reduce((acc, curr) =>
            ({ ...acc, [curr.code]: curr.description }), {});
    
        return htmlDoc
    })();
    
    document.querySelector("#Nationality").innerHTML =
        Object.entries(languageCodes).reduce((x, [key, value]) => 
            x + '<option value="' + key + '">' + value + '</option>',
            '<option value="" disabled selected>Please select</option>') +
            '<option value="O">Other</option><option disabled></option><option value="NA">Do not wish to respond</option>';

    createSubmissionArray("dem-submit", "select", demographicsSQLTable, "feedback-letter", "Age");
})();
