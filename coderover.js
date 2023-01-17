var selectedTask;
var selectedTaskIndex = 0;
var scenarioIndex;

var gridWidth;
var gridHeight;
var squareSize = 40;

var canvas;
var stage;
var output;
var code;
var taskCombo;
var runButton;
var description;

var rover;

var environment;
var controller;

var animationSteps = [];

function init() {
    canvas = document.getElementById("canvas");

    stage = new createjs.Stage("canvas");

    rover = new createjs.Shape();
    rover.graphics.beginStroke("red")
        .moveTo(-2, -15)
        .lineTo(2, -15)
        .lineTo(15, 15)
        .lineTo(-15, 15)
        .lineTo(-2, -15);

    createjs.Ticker.addEventListener("tick", stage);

    taskCombo = document.getElementById("tasks");    

    setGridSize(12, 12);

    environment = new Environment(gridWidth, gridHeight);

    code = document.getElementById("code");

    output = document.getElementById("output");

    description = document.getElementById("description");

    runButton = document.getElementById("run");

    fetch('./tasks.json')
        .then((response) => response.json())
        .then((json) => { 
            tasks = json;
            tasks.forEach(function (currentValue, index) {
                var opt = document.createElement('option');
                opt.value = index;
                opt.innerHTML = currentValue.title;
                taskCombo.appendChild(opt);
            })
            loadSelectedTask(); // This is none, but rover gets rendered
        });
}

function setGridSize(width, height) {
    gridWidth = width;
    gridHeight = height;
    canvas.width = gridWidth * squareSize;
    canvas.height = gridHeight * squareSize;
}

function addObstruction(x, y) {
    environment.addObstruction(x, y);
    renderObstruction(x, y);
}

function addPaint(x, y) {
    environment.paint(x, y);
    renderPaint(x, y);
}

function addPaints(coords) {
    var maxy = coords.y + (coords.dy ?? 1);
    var maxx = coords.x + (coords.dx ?? 1);
    for (var y = coords.y; y < maxy; y++) {
        for (var x = coords.x; x < maxx; x++) {
            addPaint(x, y)
        }
    }   
}

function addObstructions(obstruction) {
    var maxy = obstruction.y + (obstruction.dy ?? 1);
    var maxx = obstruction.x + (obstruction.dx ?? 1);
    for (var y = obstruction.y; y < maxy; y++) {
        for (var x = obstruction.x; x < maxx; x++) {
            addObstruction(x, y)
        }
    }
}

function loadSelectedTask() {
    if (selectedTask) {
        stage.removeAllChildren();
        stage.update();
        setGridSize(selectedTask.gridSize.width, selectedTask.gridSize.height);

        environment = new Environment(gridWidth, gridHeight);

        if (selectedTask.obstructions) {
            selectedTask.obstructions.forEach(addObstructions);
        }
        if (selectedTask.paint) {
            selectedTask.paint.forEach(addPaints);
        }
        if (selectedTask.circles) {
            selectedTask.circles.forEach(renderCircle);
        }
        if (selectedTask.flags) {
            selectedTask.flags.forEach(renderFlag);
        }
        if (selectedTask.description) {
            description.innerHTML = selectedTask.description;
        }

        loadScenario();
    } else {
        syncState(new State(2, 2, 0));
        stage.addChild(rover);
        stage.update();
    }
}

function loadTaskByIndex(select) {
    if (select.value) {
        var oldCode = localStorage.getItem(selectedTaskIndex);
        if (!oldCode) {
            localStorage.setItem(selectedTaskIndex, code.value);
        }
        selectedTaskIndex = select.value;
        selectedTask = tasks[selectedTaskIndex];
        scenarioIndex = 0;
        code.value = localStorage.getItem(selectedTaskIndex);
        loadSelectedTask();
    }
}

function loadScenario() {
    var currentScenario = selectedTask.scenarios[scenarioIndex];
    syncState(new State(
        currentScenario.startState.gridX,
        currentScenario.startState.gridY,
        currentScenario.startState.directionIndex
    ));
    stage.addChild(rover);
    stage.update();

    if (currentScenario.obstructions) {
        currentScenario.obstructions.forEach(addObstructions);
    }
    if (currentScenario.paint) {
        currentScenario.paint.forEach(addPaints);
    }
    if (selectedTask.circles) {
        selectedTask.circles.forEach(renderCircle);
    }
    if (currentScenario.flags) {
        currentScenario.flags.forEach(renderFlag);
    }    
}

function evalIsMoveSafe() {
    if (selectedTask && selectedTask.isMoveSafe) {
        if (eval(selectedTask.isMoveSafe.condition)) {
            return true;
        } else {
            appendOutput(selectedTask.isMoveSafe.message);
            return false;
        }
    }
    return true;
}

function runAnimationStepAtIndex(index) {
    if (index < animationSteps.length) {
        var step = animationSteps[index];
        if (step.toString().startsWith("State(")) {
            createjs.Tween.get(rover)
                .to({
                    x: step.gridX * squareSize + (squareSize / 2),
                    y: step.gridY * squareSize + (squareSize / 2),
                    rotation: step.directionIndex * 90
                },
                    1,
                    createjs.Ease.getPowInOut(4))
                .call(function () {
                    if (evalIsMoveSafe) {
                        runAnimationStepAtIndex(index + 1);
                    }
                });
        } else if (step.toString().startsWith("PaintEvent(")) {
            renderPaint(step.gridX, step.gridY);
            runAnimationStepAtIndex(index + 1);
        } else if (step.toString().startsWith("PrintEvent(")) {
            appendOutput(step.text);
            runAnimationStepAtIndex(index + 1);
        }
    } else {
        onCompleteScenario();
        toggleEditability(true);
    }
}

function toggleEditability(enabled) {
    code.disabled = !enabled;
    taskCombo.disabled = !enabled;
    runButton.disabled = !enabled;
}

function appendOutput(text) {
    output.value += text + "\n";
    output.scrollTop = output.scrollHeight;
}

function onCompleteScenario() {
    if (evalIsMoveSafe()) {
        if (selectedTask && selectedTask.isComplete) {
            if (eval(selectedTask.isComplete)) {
                scenarioIndex++;
                if (scenarioIndex < selectedTask.scenarios.length) {
                    appendOutput("SCENARIO COMPLETE");
                    loadSelectedTask();
                    runTask();
                } else {
                    appendOutput("TASK COMPLETE");
                }
            } else {
                appendOutput("SCENARIO FAILED");
            }
        }
    }
}

class JSDelegate {
    appendStep(step) {
        animationSteps.push(step);
    }
}

function syncState(state) {
    controller = new Controller(
        state,
        new JSDelegate(),
        environment
    );
    rover.x = state.gridX * squareSize + (squareSize / 2);
    rover.y = state.gridY * squareSize + (squareSize / 2);
    rover.rotation = state.directionIndex * 90;
    stage.update();
}

function renderCircle(coords) {
    var circle = new createjs.Shape();
    circle.graphics
        .beginStroke("green")
        .drawCircle(
            coords.x * squareSize + (squareSize / 2),
            coords.y * squareSize + (squareSize / 2),
            squareSize / 2 - 1);
    stage.addChild(circle);  
}

function renderFlag(coords) {
    var flag = new createjs.Shape();
    flag.graphics
        .beginStroke("blue")
            .moveTo(15, 35)
            .lineTo(15, 5)
            .lineTo(35, 10)
            .lineTo(15, 20)
    stage.addChild(flag);
    flag.setTransform(coords.x * squareSize, coords.y * squareSize);  
}

function renderObstruction(x, y) {
    var paint = new createjs.Shape();
    paint.graphics
        .beginFill("#000000")
        .drawRect(x * squareSize + 1, y * squareSize + 1, squareSize - 1, squareSize - 1);
    stage.addChild(paint);
}

function renderPaint(x, y) {
    var paint = new createjs.Shape();
    paint.graphics
        .beginFill("#ffff00")
        .drawRect(x * squareSize + 1, y * squareSize + 1, squareSize - 1, squareSize - 1);
    stage.addChild(paint);
    stage.setChildIndex(paint, 0);
}

function doRun() {
    scenarioIndex = 0;
    output.value = "";
    toggleEditability(false);
    loadSelectedTask();
    runTask();
}

function runTask() {
    var parser = new LanguageParser();

    localStorage.setItem(selectedTaskIndex, code.value);

    var parseResult = parser.parse(code.value);

    appendOutput(parseResult.toString());

    if (parseResult.s_util_parsing_combinator_Parsers$Success__f_successful) {
        var evaluator = new Evaluator();

        animationSteps = [];

        var evalResult = evaluator.evaluate(parseResult.s_util_parsing_combinator_Parsers$Success__f_result,
            controller);

        appendOutput(evalResult.toString());

        runAnimationStepAtIndex(0);
    } else {
        toggleEditability(true);
    }
}