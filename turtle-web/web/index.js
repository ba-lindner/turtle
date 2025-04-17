$(function() {
    function append_hist(kind, content) {
        $('#history').append(`<span class="hist-line" kind="${kind}">${content}</span><br/>`);
        $('#history>br:last-child')[0].scrollIntoView(false);
    }

    $('.terminal').on('click', function() {
        $('#term-input').focus();
    });

    const api = 'http://localhost:8000';
    $('#term-input').on('keydown', function(e) {
        if (e.keyCode == 13) {
            const cmd = $(this).val();
            append_hist('inp', '> ' + cmd);
            $.post(api + '/exec', cmd);
            $('#term-input').val('');
        }
    });

    const canvas = $('#turtle-window')[0];
    const ctx = canvas.getContext('2d');
    const width = canvas.width;
    const height = canvas.height;

    function map_coord(coord) {
        console.log('coordinate: ' + coord);
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

    setInterval(() => {
        $.getJSON(api + '/output', function(data) {
            for (const line of data) {
                const msg = $('<div>').text(line.msg).html()
                append_hist(line.kind, msg);
            }
        })
        $.getJSON(api + '/lines', function(data) {
            for (const line of data) {
                const from = map_coord(line.start);
                const to = map_coord(line.end);
                ctx.strokeStyle = map_col(line.color);
                ctx.beginPath();
                ctx.moveTo(from[0], from[1]);
                ctx.lineTo(to[0], to[1]);
                ctx.stroke();
            }
        })
    }, 20)
});