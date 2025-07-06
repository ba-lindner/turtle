$(function() {
    const runId = new URLSearchParams(window.location.search).get("id")

    function append_hist(kind, content) {
        const div = $('<div>').text(content).html()
        $('#history').append(`<span class="hist-line" kind="${kind}">${div}</span><br/>`);
        $('#history>br:last-child')[0].scrollIntoView(false);
    }

    $('.terminal').on('click', function() {
        $('#term-input').focus();
    });

    $('#term-input').on('keydown', function(e) {
        if (e.keyCode == 13) {
            const cmd = $(this).val();
            append_hist('inp', '> ' + cmd);
            $.ajax(`/run/${runId}/debug?formatted=true`, {
                type: "POST",
                data: JSON.stringify(cmd),
                contentType: "application/json",
                success: function(res) {
                    if (res.output != null) {
                        append_hist("out", res.output);
                    }
                    if (res.error != null) {
                        append_hist("err", res.error);
                    }
                    if (res.stmt_count != null) {
                        append_hist("out", `executed ${res.stmt_count} statements`)
                    }
                    for (const evt of res.events) {
                        if (evt.turtle_finished != null) {
                            append_hist("out", `turtle #${evt.turtle_finished} finished`)
                        } else if (evt.breakpoint != null) {
                            append_hist("out", `breakpoint #${evt.breakpoint} hit`)
                        }
                    }
                    if (res.end != null) {
                        clearInterval(drawId)
                    }
                }
            });
            $('#term-input').val('');
        }
    });

    const canvas = $('#turtle-window')[0];
    const ctx = canvas.getContext('2d');
    const width = canvas.width;
    const height = canvas.height;

    function map_coord(coord) {
        return [
            (1 + coord[0]) / 2 * width,
            (1 - coord[1]) / 2 * height,
        ];
    }

    function map_col(col) {
        const r = Math.floor(col[0] * 2.55);
        const g = Math.floor(col[1] * 2.55);
        const b = Math.floor(col[2] * 2.55);
        return `rgb(${r} ${g} ${b})`;
    }

    let collected = 0

    const drawId = setInterval(() => {
        $.getJSON(`/run/${runId}/window?skip=${collected}`, function(data) {
            collected += data.length
            for (const line of data) {
                switch (line.kind) {
                    case "draw":
                        const from = map_coord(line.from);
                        const to = map_coord(line.to);
                        ctx.strokeStyle = map_col(line.col);
                        ctx.beginPath();
                        ctx.moveTo(from[0], from[1]);
                        ctx.lineTo(to[0], to[1]);
                        ctx.stroke();
                        break
                    case "clear":
                        ctx.clearRect(0, 0, width, height)
                        break
                    case "print":
                        append_hist("ttl", line.msg)
                        break
                }
            }
        })
    }, 20)
});