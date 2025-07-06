function runExample(ex) {
    $.ajax(`/examples/${ex}/run`, {
        type: "POST",
        data: JSON.stringify([]),
        contentType: "application/json",
        success: function (uuid) {
            window.location.href = `/viewRun.html?id=${uuid}`
        }
    })
}

$(function () {
    function appendGroup(group) {
        $("#example-list").append(`<h3>${group}</h3>`)
    }

    function appendExample(ex) {
        let div = '<div class="example">'
            + `<button class="run" onClick="runExample('${ex.name}')">⏵<span class="tooltip">Run Example</span></button>`
            + `<span class="name">${ex.name}</span>`
            + "<br>"
            + `<span class="descr">${ex.summary}</span>`
            + "</div>"
        $("#example-list").append(div)
    }

    $.getJSON("/examples", function (data) {
        let lastGroup = ""
        for (const ex of data) {
            if (ex.group != lastGroup) {
                lastGroup = ex.group
                appendGroup(ex.group)
            }
            appendExample(ex)
        }
    })
});