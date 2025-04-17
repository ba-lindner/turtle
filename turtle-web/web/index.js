$(function() {
    $('.terminal').on('click', function() {
        $('#term-input').focus();
    });

    const api = 'http://localhost:8000';
    $('#term-input').on('keydown', function(e) {
        if (e.keyCode == 13) {
            const cmd = $(this).val();
            $('#history').append('> ' + cmd + '<br/>');
            $.post(api + '/exec', cmd)
            $('#term-input').val('');
        }
    });

    setInterval(() => {
        $.getJSON(api + '/output', function(data) {
            for (const line of data) {
                if (line.kind == "ok") {
                    $('#history').append(line.msg + '<br/>');
                } else {
                    $('#history').append('<span class="err">' + line.msg + '</span><br/>');
                }
            }
        })
    }, 20)
});