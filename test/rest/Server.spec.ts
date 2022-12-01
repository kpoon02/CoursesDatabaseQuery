import Server from "../../src/rest/Server";
import InsightFacade from "../../src/controller/InsightFacade";
import chai, {expect, use} from "chai";
import chaiHttp from "chai-http";
import * as fs from "fs-extra";
import {InsightDatasetKind} from "../../src/controller/IInsightFacade";

describe("Server", function () {
	let facade: InsightFacade;
	let server: Server;
	const persistDirectory = "./data";

	use(chaiHttp);

	before(async function () {
		facade = new InsightFacade();
		server = new Server(4321);
		fs.removeSync(persistDirectory);
		try {
			await server.start();
		} catch (err) {
			console.log("Server could not start");
		}
	});

	after(function () {
		server.stop();
		fs.removeSync(persistDirectory);
	});

	beforeEach(function () {
		// might want to add some process logging here to keep track of what"s going on
		console.info(`BeforeTest: ${this.currentTest?.title}`);
	});

	afterEach(function () {
		// might want to add some process logging here to keep track of what"s going on
		console.info(`AfterTest: ${this.currentTest?.title}`);
	});

	it("Successful PUT test for courses dataset - sections", function () {
		try {
			return chai
				.request("http://localhost:4321")
				.put("/dataset/sections/sections")
				.send(fs.readFileSync("./test/resources/archives/pair.zip"))
				.set("Content-Type", "application/x-zip-compressed")
				.then(function (res: ChaiHttp.Response) {
					// some logging here please!
					console.log("Response received");
					expect(res.status).to.be.equal(200);
					console.log(res.body);
					expect(res.body).to.deep.equal({result: ["sections"]});
				})
				.catch(function (err) {
					// some logging here please!
					console.log(err);
					expect.fail();
				});
		} catch (err) {
			// and some more logging here!
			expect.fail("unexpected error for PUT");
		}
	});

	it("Successful PUT test for courses dataset - rooms", function () {
		try {
			return chai
				.request("http://localhost:4321")
				.put("/dataset/rooms/rooms")
				.send(fs.readFileSync("./test/resources/archives/rooms.zip"))
				.set("Content-Type", "application/x-zip-compressed")
				.then(function (res: ChaiHttp.Response) {
					// some logging here please!
					console.log("Response received");
					expect(res.status).to.be.equal(200);
					expect(res.body).to.deep.equal({result: ["sections", "rooms"]});
				})
				.catch(function (err) {
					// some logging here please!
					console.log(err);
					expect.fail();
				});
		} catch (err) {
			// and some more logging here!
			expect.fail("unexpected error for PUT");
		}
	});

	it("Unsuccessful PUT test for courses dataset - wrong dataset kind for rooms", function () {
		try {
			return chai
				.request("http://localhost:4321")
				.put("/dataset/rooms/sections")
				.send(fs.readFileSync("./test/resources/archives/rooms.zip"))
				.set("Content-Type", "application/x-zip-compressed")
				.then(function (res: ChaiHttp.Response) {
					// some logging here please!
					console.log("Response received");
					expect(res.status).to.be.equal(400);
					expect(res.body).to.have.property("error");
				});
		} catch (err) {
			// and some more logging here!
			expect.fail("unexpected error for PUT");
		}
	});

	it("Unsuccessful PUT test for courses dataset - wrong dataset kind for sections", function () {
		try {
			return chai
				.request("http://localhost:4321")
				.put("/dataset/sections/rooms")
				.send(fs.readFileSync("./test/resources/archives/pair.zip"))
				.set("Content-Type", "application/x-zip-compressed")
				.then(function (res: ChaiHttp.Response) {
					// some logging here please!
					console.log("Response received");
					expect(res.status).to.be.equal(400);
					expect(res.body).to.have.property("error");
				});
		} catch (err) {
			// and some more logging here!
			expect.fail("unexpected error for PUT");
		}
	});

	it("Unsuccessful PUT test for courses dataset - rooms with empty id", function () {
		try {
			return chai
				.request("http://localhost:4321")
				.put("/dataset//rooms")
				.send(fs.readFileSync("./test/resources/archives/rooms.zip"))
				.set("Content-Type", "application/x-zip-compressed")
				.then(function (res: ChaiHttp.Response) {
					// some logging here please!
					console.log("Response received");
					expect(res.status).to.be.equal(404);
				});
		} catch (err) {
			// and some more logging here!
			expect.fail("unexpected error for PUT");
		}
	});

	it("Unsuccessful PUT test for courses dataset - rooms with just spaces id", function () {
		try {
			return chai
				.request("http://localhost:4321")
				.put("/dataset/ /rooms")
				.send(fs.readFileSync("./test/resources/archives/rooms.zip"))
				.set("Content-Type", "application/x-zip-compressed")
				.then(function (res: ChaiHttp.Response) {
					// some logging here please!
					console.log("Response received");
					expect(res.status).to.be.equal(400);
					expect(res.body).to.have.property("error");
				});
		} catch (err) {
			// and some more logging here!
			expect.fail("unexpected error for PUT");
		}
	});

	it("Unsuccessful PUT test for courses dataset - Underscore in dataset name", function () {
		try {
			return chai
				.request("http://localhost:4321")
				.put("/dataset/sections_/sections")
				.send(fs.readFileSync("./test/resources/archives/pair.zip"))
				.set("Content-Type", "application/x-zip-compressed")
				.then(function (res: ChaiHttp.Response) {
					// some logging here please!
					console.log("Response received");
					expect(res.status).to.be.equal(400);
					expect(res.body).to.have.property("error");
				});
		} catch (err) {
			// and some more logging here!
			expect.fail("unexpected error for PUT");
		}
	});

	it("Unsuccessful PUT test for courses dataset - Adding same id dataset", async function () {
		await facade.addDataset(
			"section1",
			fs.readFileSync("./test/resources/archives/pair.zip").toString("base64"),
			InsightDatasetKind.Sections
		);
		try {
			return chai
				.request("http://localhost:4321")
				.put("/dataset/section1/sections")
				.send(fs.readFileSync("./test/resources/archives/pair.zip"))
				.set("Content-Type", "application/x-zip-compressed")
				.then(function (res: ChaiHttp.Response) {
					// some logging here please!
					console.log("Response received");
					expect(res.status).to.be.equal(400);
					expect(res.body).to.have.property("error");
				});
		} catch (err) {
			// and some more logging here!
			expect.fail("unexpected error for PUT");
		}
	});

	it("Successful DELETE test for courses dataset", async function () {
		await facade.addDataset(
			"testSection",
			fs.readFileSync("./test/resources/archives/pair.zip").toString("base64"),
			InsightDatasetKind.Sections
		);
		try {
			return chai
				.request("http://localhost:4321")
				.delete("/dataset/testSection")
				.then(function (res: ChaiHttp.Response) {
					// some logging here please!
					console.log("Response received");
					expect(res.status).to.be.equal(200);
					expect(res.body).to.deep.equal({result: "testSection"});
				});
		} catch (err) {
			// and some more logging here!
			expect.fail("unexpected error for DELETE");
		}
	});

	it("Unsuccessful DELETE test for courses dataset - 404 Not found error", async function () {
		try {
			return chai
				.request("http://localhost:4321")
				.delete("/dataset/section")
				.then(function (res: ChaiHttp.Response) {
					// some logging here please!
					console.log("Response received");
					expect(res.status).to.be.equal(404);
				});
		} catch (err) {
			// and some more logging here!
			expect.fail("unexpected error for PUT");
		}
	});

	it("Unsuccessful DELETE test for courses dataset - underscore in id", async function () {
		try {
			return chai
				.request("http://localhost:4321")
				.delete("/dataset/_section")
				.then(function (res: ChaiHttp.Response) {
					// some logging here please!
					console.log("Response received");
					expect(res.status).to.be.equal(400);
					expect(res.body).to.have.property("error");
				});
		} catch (err) {
			// and some more logging here!
			expect.fail("unexpected error for DELETE");
		}
	});

	it("Unsuccessful DELETE test for courses dataset - spaces only id", async function () {
		try {
			return chai
				.request("http://localhost:4321")
				.delete("/dataset/ /")
				.then(function (res: ChaiHttp.Response) {
					// some logging here please!
					console.log("Response received");
					expect(res.status).to.be.equal(400);
					expect(res.body).to.have.property("error");
				});
		} catch (err) {
			// and some more logging here!
			expect.fail("unexpected error for DELETE");
		}
	});

	it("Successful POST test for courses dataset - section query", function () {
		try {
			return chai
				.request("http://localhost:4321")
				.post("/query")
				.send({
					WHERE: {
						GT: {
							sections_avg: 97,
						},
					},
					OPTIONS: {
						COLUMNS: ["sections_dept", "sections_avg"],
						ORDER: "sections_avg",
					},
				})
				.then(function (res: ChaiHttp.Response) {
					// some logging here please!
					console.log("Response received");
					expect(res.status).to.be.equal(200);
				})
				.catch(function (err) {
					// some logging here please!
					console.log(err);
					expect.fail();
				});
		} catch (err) {
			// and some more logging here!
			expect.fail("unexpected error for POST");
		}
	});

	it("Unsuccessful POST test for dataset - section query", function () {
		try {
			return chai
				.request("http://localhost:4321")
				.post("/query")
				.send({
					WHERE: {
						GT: {
							sections_avg: 97,
						},
					},
					OPTIONS: {
						COLUMNS: ["sections_dept"],
						ORDER: "sections_avg",
					},
				})
				.then(function (res: ChaiHttp.Response) {
					// some logging here please!
					console.log("Response received");
					expect(res.status).to.be.equal(400);
				})
				.catch(function (err) {
					// some logging here please!
					console.log(err);
					expect.fail();
				});
		} catch (err) {
			// and some more logging here!
			expect.fail("unexpected error for POST");
		}
	});

	it("Successful POST test for dataset - room query", function () {
		try {
			return chai
				.request("http://localhost:4321")
				.post("/query")
				.send({
					WHERE: {
						AND: [
							{
								IS: {
									rooms_furniture: "*Tables*",
								},
							},
							{
								GT: {
									rooms_seats: 300,
								},
							},
						],
					},
					OPTIONS: {
						COLUMNS: ["rooms_shortname", "maxSeats"],
						ORDER: {
							dir: "DOWN",
							keys: ["maxSeats"],
						},
					},
					TRANSFORMATIONS: {
						GROUP: ["rooms_shortname"],
						APPLY: [
							{
								maxSeats: {
									MAX: "rooms_seats",
								},
							},
						],
					},
				})
				.then(function (res: ChaiHttp.Response) {
					// some logging here please!
					console.log("Response received");
					expect(res.status).to.be.equal(200);
				});
		} catch (err) {
			// and some more logging here!
			expect.fail("unexpected error for POST");
		}
	});

	it("Successful POST test for dataset - room query", function () {
		try {
			return chai
				.request("http://localhost:4321")
				.post("/query")
				.send({
					WHERE: {
						AND: [
							{
								IS: {
									rooms_furniture: "*Tables*",
								},
							},
							{
								GT: {
									rooms_seats: 300,
								},
							},
						],
					},
					OPTIONS: {
						COLUMNS: ["rooms_shortname", "maxSeats"],
						ORDER: {
							dir: "DOWN",
						},
					},
				})
				.then(function (res: ChaiHttp.Response) {
					// some logging here please!
					console.log("Response received");
					expect(res.status).to.be.equal(400);
				});
		} catch (err) {
			// and some more logging here!
			expect.fail("unexpected error for POST");
		}
	});

	it("Successful GET test for courses dataset", function () {
		try {
			return chai
				.request("http://localhost:4321")
				.get("/datasets")
				.then(function (res: ChaiHttp.Response) {
					// some logging here please!
					console.log("Response received");
					expect(res.status).to.be.equal(200);
				});
		} catch (err) {
			// and some more logging here!
			expect.fail("unexpected error for GET");
		}
	});

	it("Successful GET test for courses dataset - Case insensitive url", function () {
		try {
			return chai
				.request("http://localhost:4321")
				.get("/DataSEts")
				.then(function (res: ChaiHttp.Response) {
					// some logging here please!
					console.log("Response received");
					expect(res.status).to.be.equal(200);
				});
		} catch (err) {
			// and some more logging here!
			expect.fail("unexpected error for GET");
		}
	});

	it("Unsuccessful GET test for courses dataset - wrong url", function () {
		try {
			return chai
				.request("http://localhost:4321")
				.get("/dataset")
				.then(function (res: ChaiHttp.Response) {
					// some logging here please!
					console.log("Response received");
					expect(res.status).to.be.equal(404);
				});
		} catch (err) {
			// and some more logging here!
			expect.fail("unexpected error for GET");
		}
	});

	// Sample on how to format PUT requests
	/*
	it("PUT test for courses dataset", function () {
		try {
			return chai.request(SERVER_URL)
				.put(ENDPOINT_URL)
				.send(ZIP_FILE_DATA)
				.set("Content-Type", "application/x-zip-compressed")
				.then(function (res: ChaiHttp.Response) {
					// some logging here please!
					expect(res.status).to.be.equal(200);
				})
				.catch(function (err) {
					// some logging here please!
					expect.fail();
				});
		} catch (err) {
			// and some more logging here!
		}
	});
	*/

	// The other endpoints work similarly. You should be able to find all instructions at the chai-http documentation
});
