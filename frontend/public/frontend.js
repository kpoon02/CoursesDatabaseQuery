const BASE_QUERY = {
	"WHERE": {},
	"OPTIONS": {
		"COLUMNS": []
	}
}

const filterButtonIds = [
	"lt-filter-button",
	"gt-filter-button",
	"eq-filter-button",
	"is-filter-button",
	"not-filter-button",
	"and-filter-button",
	"or-filter-button"
]

const lcompButtonIds = [
	"and-filter-button",
	"or-filter-button"
]

const historyButtonIds = [
	"history-1-button",
	"history-2-button",
	"history-3-button",
	"history-4-button",
	"history-5-button",
	"clear-history-button"
]

let history = []
let query = structuredClone(BASE_QUERY)
let parentQuery = query["WHERE"]
let modal = document.getElementById("myModal")

document.getElementById("submit-query-button").addEventListener("click", handleSubmitButton);
document.getElementById("lt-filter-button").addEventListener("click", () => {
	handleLtButton(parentQuery)
	btnResolver()
});
document.getElementById("gt-filter-button").addEventListener("click", () => {
	handleGtButton(parentQuery)
	btnResolver()
});
document.getElementById("eq-filter-button").addEventListener("click", () => {
	handleEqButton(parentQuery)
	btnResolver()
});
document.getElementById("is-filter-button").addEventListener("click", () => {
	handleIsButton(parentQuery)
	btnResolver()
});
document.getElementById("not-filter-button").addEventListener("click", () => {
	handleNotButton(parentQuery)
});
document.getElementById("and-filter-button").addEventListener("click", async () => {
	await handleAndButton(parentQuery)
});
document.getElementById("or-filter-button").addEventListener("click", async () => {
	await handleOrButton(parentQuery)
});
document.getElementById("history-button").addEventListener("click", async () => {
	await retrieveHistory()
});
document.getElementById("clear-history-button").addEventListener("click", clearHistory);
document.getElementById("history-1-button").addEventListener("click", async () => {
	await displayHistory(0)
});
document.getElementById("history-2-button").addEventListener("click", async () => {
	await displayHistory(1)
});
document.getElementById("history-3-button").addEventListener("click", async () => {
	await displayHistory(2)
});
document.getElementById("history-4-button").addEventListener("click", async () => {
	await displayHistory(3)
});
document.getElementById("history-5-button").addEventListener("click", async () => {
	await displayHistory(4)
});

let waitForPressResolve;

function waitForPress() {
	return new Promise(resolve => waitForPressResolve = resolve);
}

function btnResolver() {
	if (waitForPressResolve) waitForPressResolve();
}

function displayQuery() {
	document.getElementById("query-json-display").innerHTML = JSON.stringify(query, null, " ")
}

async function handleSubmitButton() {
	const response = await fetch('http://localhost:4321/query', {
		method: 'POST',
		body: JSON.stringify(query),
		headers: {
			'Content-Type': 'application/json'
		}
	});
	const result = await response.json();
	if (response.status === 200) {
		alert("Query submitted!")
		displayResults(query["OPTIONS"]["COLUMNS"], result)
	} else {
		alert("Invalid query submitted!")
	}
	resetElements()
}

async function handleHistoryQuery() {
	const response = await fetch('http://localhost:4321/history/query', {
		method: 'POST',
		body: JSON.stringify(query),
		headers: {
			'Content-Type': 'application/json'
		}
	});
	const result = await response.json();
	if (response.status === 200) {
		displayResults(query["OPTIONS"]["COLUMNS"], result)
	} else {
		alert("Invalid query submitted!")
	}
	resetElements()
}

function resetElements() {
	query = structuredClone(BASE_QUERY)
	retainCheckboxes()
	parentQuery = query["WHERE"]
	enableButtons(filterButtonIds)
}

function handleMcompInputs(parentQuery, mcompType) {
	const key = prompt("Enter a key:")
	const value = parseInt(prompt("Enter a number:"))
	parentQuery[mcompType] = {
		[key]: value
	}
	displayQuery()
}

function handleLtButton(parent) {
	handleMcompInputs(parent, "LT")
	disableButtons(filterButtonIds)
}

function handleGtButton(parent) {
	handleMcompInputs(parent, "GT")
	disableButtons(filterButtonIds)
}

function handleEqButton(parent) {
	handleMcompInputs(parent, "EQ")
	disableButtons(filterButtonIds)
}

function handleIsButton(parent) {
	const key = prompt("Enter a key:")
	const value = prompt("Enter an input string:")
	parent["IS"] = {
		[key]: value
	}
	disableButtons(filterButtonIds)
	displayQuery()
}

function handleNotButton(parent) {
	parent["NOT"] = {}
	parentQuery = parent["NOT"]
	disableButtons(["not-filter-button"])
	displayQuery()
}

async function handleLcompInputs(parent, lcompType) {
	parent[lcompType] = [{}, {}]
	parentQuery = parent[lcompType][0]
	displayQuery()
	disableButtons(lcompButtonIds)
	await waitForPress()
	enableButtons(filterButtonIds)
	disableButtons(lcompButtonIds)
	parentQuery = parent[lcompType][1]
	displayQuery()
	await waitForPress()
	disableButtons(filterButtonIds)
	displayQuery()
}

async function handleAndButton(parent) {
	await handleLcompInputs(parent, "AND")
}

async function handleOrButton(parent) {
	await handleLcompInputs(parent, "OR")
}

function addSectionColumns() {
	const roomColumns = document.getElementsByName("rooms-columns")
	for (let i = 0; i < roomColumns.length; i++) {
		roomColumns[i].checked = false
	}
	const sectionColumns = document.getElementsByName("sections-columns")
	const columns = []
	for (let i = 0; i < sectionColumns.length; i++) {
		if (sectionColumns[i].checked) {
			columns.push("sections_" + sectionColumns[i].value)
		}
	}
	query["OPTIONS"]["COLUMNS"] = columns
	displayQuery()
}

function addRoomColumns() {
	const sectionColumns = document.getElementsByName("sections-columns")
	for (let i = 0; i < sectionColumns.length; i++) {
		sectionColumns[i].checked = false
	}
	const roomColumns = document.getElementsByName("rooms-columns")
	const columns = []
	for (let i = 0; i < roomColumns.length; i++) {
		if (roomColumns[i].checked) {
			columns.push("rooms_" + roomColumns[i].value)
		}
	}
	query["OPTIONS"]["COLUMNS"] = columns
	displayQuery()
}

function retainCheckboxes() {
	const sectionColumns = document.getElementsByName("sections-columns")
	for (let i = 0; i < sectionColumns.length; i++) {
		if (sectionColumns[i].checked) {
			query["OPTIONS"]["COLUMNS"].push("sections_" + sectionColumns[i].value)
		}
	}
	const roomColumns = document.getElementsByName("rooms-columns")
	for (let i = 0; i < roomColumns.length; i++) {
		if (roomColumns[i].checked) {
			query["OPTIONS"]["COLUMNS"].push("rooms_" + roomColumns[i].value)
		}
	}
}

function disableButtons(buttons) {
	for (let buttonId of buttons) {
		document.getElementById(buttonId).style.backgroundColor = "#A9A9A9"
		document.getElementById(buttonId).disabled = true
	}
}

function enableButtons(buttons) {
	for (let buttonId of buttons) {
		document.getElementById(buttonId).style.backgroundColor = "#3f51b5"
		document.getElementById(buttonId).disabled = false
	}
}

function displayResults(columns, results) {
	results = results.result
	let content = "<table><thead><tr>"
	for (let column of columns) {
		content += "<th>" + column.split("_")[1] + "</th>"
	}
	content += "</tr></thead><tbody>"
	for (let result of results) {
		content += "<tr onclick='showDetails(\"" + result["sections_uuid"] + "\")'>"
		for (let column of columns) {
			content += "<td>" + result[column] + "</td>"
		}
		content += "</tr>"
	}
	document.getElementById("query-result-display").innerHTML = content
}

async function showDetails(uuid) {
	if (uuid === "undefined") {
		alert("Results must include the UUID (Sections) or the Short Name (Rooms) in order to view full details!")
	} else {
		const detailQuery = {
			"WHERE": {
				"IS": {
					"sections_uuid": uuid
				}
			},
			"OPTIONS": {
				"COLUMNS": [
					"sections_dept",
					"sections_id",
					"sections_avg",
					"sections_instructor",
					"sections_title",
					"sections_pass",
					"sections_fail",
					"sections_audit",
					"sections_uuid",
					"sections_year"
				]
			}
		}
		const response = await fetch('http://localhost:4321/query', {
			method: 'POST',
			body: JSON.stringify(detailQuery),
			headers: {
				'Content-Type': 'application/json'
			}
		});
		let result = await response.json();

		result = result.result
		let text = ""
		for (let key of Object.keys(result[0])) {
			text += "<b>" + key.split("_")[1] + ":</b> " + result[0][key] + "<br>"
		}
		document.getElementById("modal-text").innerHTML = text
		let span = document.getElementsByClassName("close")[0]
		modal.style.display = "block"
		span.onclick = function() {
			modal.style.display = "none";
		}
	}
}

async function retrieveHistory() {
	const response = await fetch('http://localhost:4321/history');
	let result = await response.json();
	result = result.result
	history = []
	if (result.length !== 0) {
		disableButtons(historyButtonIds)
		const buttons = []
		for (let i = result.length - 1; i >= 0; i--) {
			history.push(result[i])
			buttons.push("history-" + (result.length - i) + "-button")
		}
		buttons.push("clear-history-button")
		enableButtons(buttons)
	} else {
		alert("No previous queries submitted!")
	}
}

async function clearHistory() {
	history = []
	disableButtons(historyButtonIds)
	const response = await fetch('http://localhost:4321/history', {
		method: 'DELETE'
	});
	let result = await response.json();
	result = result.result
	if (result === "Cleared") {
		alert("History cleared!")
	} else {
		alert("Error clearing history!")
	}
}

async function displayHistory(num) {
	query = history[num]
	displayQuery()
	await handleHistoryQuery()
}
