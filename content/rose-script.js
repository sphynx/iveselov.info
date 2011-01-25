const DICE_NUMBER = 5;
var lastDice = [];

function getRandomBetween(a, b) {
    return Math.floor(Math.random() * (b - a + 1) + a);
}

function getDice(n) {
    var arr = [], i = 0;
    for (i = 0; i < n; i++) {
        arr.push(getRandomBetween(1, 6));
    }
    return arr;
}

function roll(n) {
    var dice, i, val;
    if (n === undefined) {
        n = DICE_NUMBER;
    }

    dice = getDice(n);
    lastDice = dice;

    $("#dicebox").empty();
    $("#result").html("?");
    $("#guess").val("0");
    toggleButton(true);

    for (i = 1; i <= n; i++) {
        val = dice[i - 1];
        $("#dicebox").append("<span class='die" + val + "'/>");
    }
}

function getAnswer(dice) {
    var total = 0, i = 0;
    for (i = 0; i < dice.length; i++) {
        switch (dice[i]) {
            case 3: total += 2; break;
            case 5: total += 4; break;
        }
    }
    $("#result").html(total);
    return total;
}

function check() {
    var guess = Number($("#guess").val());
    if (guess === getAnswer(lastDice)) {
        $("#crossImg").hide();
        $("#checkImg").hide();
        $("#checkImg").fadeIn("slow");
    } else {
        $("#checkImg").hide();
        $("#crossImg").hide();
        $("#crossImg").fadeIn("slow");
    }
    toggleButton(false);
}

function toggleButton(checkState) {
    if (checkState) {
        $("#btnRoll").hide();
        $("#btnCheck").show();
    } else {
        $("#btnRoll").show();
        $("#btnCheck").hide();
    }
}

$(function() {
    roll(DICE_NUMBER);
});
