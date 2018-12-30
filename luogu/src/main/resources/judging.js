var rid = 14142956;
var flagMap = {
    12: "AC",
    3: "OLE",
    4: "MLE",
    5: "TLE",
    6: "WA",
    7: "RE"
};
var longFlagMap = {
    0: "Waiting",
    1: "Judging",
    2: "Compile Error",
    12: "Accepted",
    14: "Unaccepted",
    21: "Hack Success",
    22: "Hack Failure",
    23: "Hack Skipped"
};
var flagColorMap = {
    0: "lg-bg-bluedark",
    1: "lg-bg-bluelight",
    2: "lg-bg-yellow",
    3: "lg-bg-bluedark",
    4: "lg-bg-bluedark",
    5: "lg-bg-bluedark",
    6: "lg-bg-red",
    7: "lg-bg-purple",
    12: "lg-bg-green",
    14: "lg-bg-red",
    21: "lg-bg-green",
    22: "lg-bg-red",
    23: "lg-bg-bluedark"
};

function scoreColor(score) {
    if (score <= 20) return 'lg-fg-red';
    if (score <= 50) return 'lg-fg-orange';
    if (score <= 70) return 'lg-fg-yellow';
    return 'lg-fg-green';
}

$("#refresh-status").click(function () {
    var e = $(this);
    e.addClass("am-disabled");
    if (confirm("将从源 OJ 获取评测状态，2分钟可获取一次。")) {
        $.post("/api/record/refreshVjudgeStatus/" + rid, {}).done(function (resp) {
            e.removeClass("am-disabled");
            if (resp.status !== 200) {
                show_alert("刷新时发生错误", resp.data);
            }
        })
    } else e.removeClass("am-disabled");
})

var pointCount = 10;

function renderHackData(message, status) {
    if (status || status === 0) {
        var statusText = longFlagMap.hasOwnProperty(status) ? longFlagMap[status] : 'Unknown Error';
        var statusColor = flagColorMap.hasOwnProperty(status) ? flagColorMap[status] : 'lg-bg-black';
        $(".record-status").removeClass("lg-bg-bluelight").removeClass("lg-bg-bluedark").addClass(statusColor).text(statusText);

        if (status !== 1 && status !== 0)
            if (ws) ws.close();
    }

    $("#hack").show();
    if (message.length === 0) message = "没有信息";
    $("#hack-message").show().text(message);
}

function renderData(data, status, score, time, memory) {
    if ($.type(data) === 'string')
        return renderHackData(data, status);

    var $compileStatus = $("#compile-status"), $compileMessage = $("#compile-message");
    if (data.compile) {
        $("#compile").show();
        var compileMessage = data.compile.content;
        if (!compileMessage || compileMessage.length === 0)
            compileMessage = "没有编译信息";

        $compileMessage.show().text(compileMessage);
        if (data.compile.flag === 12) {
            $compileStatus.text("编译成功");
        } else {
            $compileStatus.text("编译失败");
        }
    } else {
        $compileStatus.text("");
    }

    if (status || status === 0) {
        var statusText = longFlagMap.hasOwnProperty(status) ? longFlagMap[status] : 'Unknown Error';
        var statusColor = flagColorMap.hasOwnProperty(status) ? flagColorMap[status] : 'lg-bg-black';
        $(".record-status").removeClass("lg-bg-bluelight").removeClass("lg-bg-bluedark").addClass(statusColor).text(statusText);

        if (status !== 1 && status !== 0) {
            if (ws) ws.close();
            $("#refresh-status").hide();
        }
        if (status === 2)
            $("#cases,#subtasks").hide();
    }

    if (score !== null && showScore)
        $(".total-score").addClass(scoreColor(score)).text(score);

    if (time === null) time = "-";
    if (memory === null) memory = "-";
    $(".record-time").text(time);
    $(".record-memory").text(memory);

    var judgedCasesCount = 0;
    $.each(data, function (i, e) {
        if (0 !== i.indexOf("case")) return;

        judgedCasesCount++;
        var caseId = parseInt(i.replace("case", ""));
        var $caseTile = $(".case-tile[data-case-id=" + caseId + "]");
        if ($caseTile.length === 0) {
            var caseHtml = '<div class="lg-record-tile case-tile" data-case-id="' + caseId + '">\n' +
                '   <small>#' + caseId + '</small>\n' +
                '   <strong class="case-status"></strong>\n' +
                '   <small class="am-text-center usage-data"></small>\n' +
                '</div>';
            $("#cases").append(caseHtml);
            $caseTile = $(".case-tile[data-case-id=" + caseId + "]");
        }

        var message = "";
        if (e.flag === 12) {
            message = "通过该测试点。";
            $caseTile.children(".usage-data").text(e.time + "ms/" + e.memory + "KB");
        }
        message += e.desc ? e.desc : '';
        if (showScore) message += " 得分" + e.score;
        $caseTile.attr('title', message);

        var statusText = flagMap.hasOwnProperty(e.flag) ? flagMap[e.flag] : 'UKE';
        $caseTile.children(".case-status").text(statusText);

        var statusColor = flagColorMap.hasOwnProperty(e.flag) ? flagColorMap[e.flag] : 'lg-bg-black';
        $caseTile.removeClass("lg-bg-bluedark").removeClass("lg-bg-bluelight").addClass(statusColor);
    });
    $.AMUI.progress.set(judgedCasesCount / pointCount);
    if (status !== 1 && status !== 0)
        $.AMUI.progress.done();

    if (status === 12) {
        $.get("/record/ajax_rate?rid=14142956", function (data) {
            var res;
            res = eval("(" + data + ")");
            if (res["code"] === 200) {
                hide_artile();
                $('#sub').html(res["more"]["html"]);
            }
        });
    }

    $(".lg-record-tile").poshytip({
        className: 'tip-twitter',
        showTimeout: 1,
        alignTo: 'target',
        alignX: 'center',
        alignY: 'top'
    });

    if (data.subtasks) {
        $.each(data.subtasks, function (i, e) {
            var $subtaskBadge = $(".subtask-badge[data-subtask-id=" + i + "]");
            var $subtaskStatus = $(".subtask-status[data-subtask-id=" + i + "]");
            var statusText = longFlagMap.hasOwnProperty(e.status) ? longFlagMap[e.status] : 'Unknown Error';
            var statusColor = flagColorMap.hasOwnProperty(e.status) ? flagColorMap[e.status] : 'lg-bg-black';

            $subtaskBadge.html('<span class="am-badge am-radius ' + statusColor + '" style="font-size:14px">' + statusText + '</span>');
            $subtaskStatus.text("总得分: " + e.score + " / 总用时: " + e.time + "ms / 内存用量: " + e.memory + "KB");
        });
    }
}

$(".sample-copy").click(function () {
    var element = $(this).parents(".copy-region").find("pre");
    var text = $(element).text();
    var $temp = $("<textarea>");
    $("body").append($temp);
    $temp.val(text).select();
    document.execCommand("copy");
    $temp.remove();
    $(this).text("复制成功").removeClass("lg-bg-orange");

    var e = this;
    setTimeout(function () {
        $(e).text("复制").addClass("lg-bg-orange");
    }, 500);
});

var ws = null;

function connectWs() {
    try {
        ws = new WebSocket('wss://ws.luogu.org/ws');
    } catch (e) {
        show_alert("错误", "无法连接追踪服务器，请定期手动刷新页面查看结果。");
        return;
    }

    ws.onopen = function () {
        var message = {
            "type": "join_channel",
            "channel": "record.track",
            "channel_param": "14142956"
        };
        ws.send(JSON.stringify(message));
    };

    ws.onmessage = function (event) {
        var data = JSON.parse(event.data);
        if (data.type === "status_push") {
            renderData(data.detail, data.status, data.score, data.time, data.memory);
        } else if (data.type === "result") {
            data = data.welcome_message;
            renderData(data.detail, data.status, data.score, data.time, data.memory);
        }
    };
}

var showScore = true;
$(document).ready(function () {
    if (ws) ws.close();
    renderData({
        "final": [],
        "init": {"version": "4"},
        "compile": {
            "content": "Free Pascal Compiler version 3.0.4+dfsg-20 [2018\/07\/02] for x86_64\nCopyright (c) 1993-2017 by Florian Klaempfl and others\nTarget OS: Linux for x86-64\nCompiling \/tmp\/tmpfcizq41q\/src\nsrc(155,6) Note: Local variable \"m\" not used\nsrc(155,8) Note: Local variable \"n\" not used\nsrc(155,10) Note: Local variable \"x\" not used\nsrc(300,12) Warning: Variable \"csmx\" does not seem to be initialized\nsrc(338,29) Warning: Variable \"p\" does not seem to be initialized\nsrc(2,9) Note: Local variable \"k\" not used\nsrc(3,5) Note: Local variable \"m\" not used\nsrc(13,11) Note: Local variable \"lx\" not used\nsrc(13,14) Note: Local variable \"ly\" not used\nLinking \/tmp\/tmpfcizq41q\/prog\n\/usr\/bin\/ld.bfd: \u8b66\u544a: \/tmp\/tmpfcizq41q\/link.res \u542b\u6709\u8f93\u51fa\u8282\uff1b\u60a8\u5fd8\u8bb0\u4e86 -T\uff1f\n361 lines compiled, 0.1 sec\n2 warning(s) issued\n7 note(s) issued\n",
            "flag": 12
        },
        "case1": {
            "flag": 12,
            "desc": "ok accepted",
            "memory": 456,
            "time": 2,
            "score": 10,
            "exit_code": 0,
            "signal": 0,
            "subtask": 0
        },
        "case3": {
            "flag": 12,
            "desc": "ok accepted",
            "memory": 704,
            "time": 3,
            "score": 10,
            "exit_code": 0,
            "signal": 0,
            "subtask": 0
        },
        "case8": {
            "flag": 12,
            "desc": "ok accepted",
            "memory": 908,
            "time": 3,
            "score": 10,
            "exit_code": 0,
            "signal": 0,
            "subtask": 0
        },
        "case6": {
            "flag": 12,
            "desc": "ok accepted",
            "memory": 776,
            "time": 2,
            "score": 10,
            "exit_code": 0,
            "signal": 0,
            "subtask": 0
        },
        "case10": {
            "flag": 12,
            "desc": "ok accepted",
            "memory": 380,
            "time": 2,
            "score": 10,
            "exit_code": 0,
            "signal": 0,
            "subtask": 0
        }
    }, 1, null, null, null);
    $("#record-region").show();
    connectWs();
    $.AMUI.progress.start();
});