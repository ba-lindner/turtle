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
            $.post(api + '/exec', cmd)
            $('#term-input').val('');
        }
    });

    setInterval(() => {
        $.getJSON(api + '/output', function(data) {
            for (const line of data) {
                const msg = $('<div>').text(line.msg).html()
                append_hist(line.kind, msg);
            }
        })
    }, 20)
});