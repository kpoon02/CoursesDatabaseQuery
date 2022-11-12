import {
	InsightDataset,
	InsightDatasetKind,
	InsightError,
	InsightResult,
	NotFoundError,
	ResultTooLargeError,
} from "../../src/controller/IInsightFacade";
import InsightFacade from "../../src/controller/InsightFacade";
import * as fs from "fs-extra";
import {folderTest} from "@ubccpsc310/folder-test";
import {expect} from "chai";
import {it} from "mocha";

describe("InsightFacade", function () {
	let insightFacade: InsightFacade;

	const persistDirectory = "./data";
	const datasetContents = new Map<string, string>();

	// Reference any datasets you've added to test/resources/archives here, and they will
	// automatically be loaded in the 'before' hook.
	const datasetsToLoad: {[key: string]: string} = {
		sections: "./test/resources/archives/pair.zip", // 64612 rows
		sectionsSomeInvalid: "./test/resources/archives/courses.zip", // 11 rows
		sectionsInvalidRootFolder: "./test/resources/archives/not_courses.zip",
		rooms: "./test/resources/archives/rooms.zip", // 364 rows
	};

	before(function () {
		// This section runs once and loads all datasets specified in the datasetsToLoad object
		for (const key of Object.keys(datasetsToLoad)) {
			const content = fs.readFileSync(datasetsToLoad[key]).toString("base64");
			datasetContents.set(key, content);
		}
		// Just in case there is anything hanging around from a previous run of the test suite
		fs.removeSync(persistDirectory);
	});

	describe("addDataset / listDataset / removeDataset", function () {
		const id: string = "sections";
		const id2: string = "sections2";
		const idSectionsSomeInvalid: string = "sectionsSomeInvalid";
		const idSectionsInvalidRootFolder: string = "sectionsInvalidRootFolder";
		const id3: string = "rooms";
		const id4: string = "rooms2";

		// before(function () {
		// 	console.info(`Before: ${this.test?.parent?.title}`);
		// });

		beforeEach(function () {
			console.info(`BeforeTest: ${this.currentTest?.title}`);
			insightFacade = new InsightFacade();
		});

		// after(function () {
		// 	console.info(`After: ${this.test?.parent?.title}`);
		// });

		afterEach(function () {
			// This section resets the data directory (removing any cached data)
			// This runs after each test, which should make each test independent of the previous one
			console.info(`AfterTest: ${this.currentTest?.title}`);
			fs.removeSync(persistDirectory);
		});

		async function addMultipleSectionDatasets() {
			const content: string = datasetContents.get("sections") ?? "";
			const content2: string = datasetContents.get("sections") ?? "";
			await insightFacade.addDataset(id, content, InsightDatasetKind.Sections);
			await insightFacade.addDataset(id2, content2, InsightDatasetKind.Sections);
			const expected1: InsightDataset = {
				id: "sections",
				kind: InsightDatasetKind.Sections,
				numRows: 64612,
			};
			const expected2: InsightDataset = {
				id: "sections2",
				kind: InsightDatasetKind.Sections,
				numRows: 64612,
			};
			return {expected1, expected2};
		}

		async function addMultipleRoomDatasets() {
			const content: string = datasetContents.get("rooms") ?? "";
			const content2: string = datasetContents.get("rooms") ?? "";
			await insightFacade.addDataset(id3, content, InsightDatasetKind.Rooms);
			await insightFacade.addDataset(id4, content2, InsightDatasetKind.Rooms);
			const expected1: InsightDataset = {
				id: "rooms",
				kind: InsightDatasetKind.Rooms,
				numRows: 364,
			};
			const expected2: InsightDataset = {
				id: "rooms2",
				kind: InsightDatasetKind.Rooms,
				numRows: 364,
			};
			return {expected1, expected2};
		}

		async function assertMultipleDatasets(expected1: InsightDataset, expected2: InsightDataset) {
			const result = await insightFacade.listDatasets();

			expect(result).to.be.instanceof(Array);
			expect(result).to.have.length(2);
			expect(result).to.deep.include(expected1);
			expect(result).to.deep.include(expected2);
		}

		it("Should add a valid section dataset", async function () {
			const expected: string[] = [id];
			const content: string = datasetContents.get("sections") ?? "";
			const result = await insightFacade.addDataset(id, content, InsightDatasetKind.Sections);

			expect(result).to.deep.equal(expected);
		});

		it("Should add a valid room dataset", async function () {
			const expected: string[] = [id3];
			const content: string = datasetContents.get("rooms") ?? "";
			const result = await insightFacade.addDataset(id3, content, InsightDatasetKind.Rooms);

			expect(result).to.deep.equal(expected);
		});

		it("Should still add sections if some are missing queryable data", async function () {
			const expected: string[] = [idSectionsSomeInvalid];
			const content: string = datasetContents.get(idSectionsSomeInvalid) ?? "";
			const result = await insightFacade.addDataset(idSectionsSomeInvalid, content, InsightDatasetKind.Sections);

			expect(result).to.deep.equal(expected);
			const result2 = await insightFacade.listDatasets();
			expect(result2[0].numRows).to.equal(11);
		});

		it("Should add multiple valid section datasets", async function () {
			const content: string = datasetContents.get("sections") ?? "";
			const content2: string = datasetContents.get("sections") ?? "";
			const expected: string[] = [id, id2];
			await insightFacade.addDataset(id, content, InsightDatasetKind.Sections);
			const result = await insightFacade.addDataset(id2, content2, InsightDatasetKind.Sections);

			expect(result).to.deep.equal(expected);
		});

		it("Should add multiple valid room datasets", async function () {
			const content: string = datasetContents.get("rooms") ?? "";
			const content2: string = datasetContents.get("rooms") ?? "";
			const expected: string[] = [id3, id4];
			await insightFacade.addDataset(id3, content, InsightDatasetKind.Rooms);
			const result = await insightFacade.addDataset(id4, content2, InsightDatasetKind.Rooms);

			expect(result).to.deep.equal(expected);
		});

		it("Should add different types of valid datasets", async function () {
			const content: string = datasetContents.get("sections") ?? "";
			const content2: string = datasetContents.get("rooms") ?? "";
			const expected: string[] = [id, id3];
			await insightFacade.addDataset(id, content, InsightDatasetKind.Sections);
			const result = await insightFacade.addDataset(id3, content2, InsightDatasetKind.Rooms);

			expect(result).to.deep.equal(expected);
		});

		it("Should reject section datasets with underscores in id", async function () {
			const invalidId: string = "sections_invalid";
			const content: string = datasetContents.get("sections") ?? "";
			try {
				await insightFacade.addDataset(invalidId, content, InsightDatasetKind.Sections);
				expect.fail("Should not have resolved");
			} catch (err) {
				expect(err).to.be.instanceof(InsightError);
			}
		});

		it("Should reject room datasets with underscores in id", async function () {
			const invalidId: string = "rooms_invalid";
			const content: string = datasetContents.get("rooms") ?? "";
			try {
				await insightFacade.addDataset(invalidId, content, InsightDatasetKind.Rooms);
				expect.fail("Should not have resolved");
			} catch (err) {
				expect(err).to.be.instanceof(InsightError);
			}
		});

		it("Should reject section datasets with only whitespaces in id", async function () {
			const invalidId: string = " ";
			const content: string = datasetContents.get("sections") ?? "";
			try {
				await insightFacade.addDataset(invalidId, content, InsightDatasetKind.Sections);
				expect.fail("Should not have resolved");
			} catch (err) {
				expect(err).to.be.instanceof(InsightError);
			}
		});

		it("Should reject room datasets with only whitespaces in id", async function () {
			const invalidId: string = " ";
			const content: string = datasetContents.get("rooms") ?? "";
			try {
				await insightFacade.addDataset(invalidId, content, InsightDatasetKind.Rooms);
				expect.fail("Should not have resolved");
			} catch (err) {
				expect(err).to.be.instanceof(InsightError);
			}
		});

		it("Should reject section datasets already on disk", async function () {
			const content: string = datasetContents.get("sections") ?? "";
			await insightFacade.addDataset(id, content, InsightDatasetKind.Sections);
			try {
				await insightFacade.addDataset(id, content, InsightDatasetKind.Sections);
				expect.fail("Should not have resolved");
			} catch (err) {
				expect(err).to.be.instanceof(InsightError);
			}
		});

		it("Should reject room datasets already on disk", async function () {
			const content: string = datasetContents.get("rooms") ?? "";
			await insightFacade.addDataset(id3, content, InsightDatasetKind.Rooms);
			try {
				await insightFacade.addDataset(id3, content, InsightDatasetKind.Rooms);
				expect.fail("Should not have resolved");
			} catch (err) {
				expect(err).to.be.instanceof(InsightError);
			}
		});

		it("Should reject zip file without 'courses' root folder", async function () {
			const content: string = datasetContents.get(idSectionsInvalidRootFolder) ?? "";
			try {
				await insightFacade.addDataset(id, content, InsightDatasetKind.Sections);
				expect.fail("Should not have resolved");
			} catch (err) {
				expect(err).to.be.instanceof(InsightError);
			}
		});

		it("Should load section datasets on disk", async function () {
			const content: string = datasetContents.get("sections") ?? "";
			await insightFacade.addDataset(id, content, InsightDatasetKind.Sections);
			const insightFacade2 = new InsightFacade();
			const expected1 = await insightFacade.listDatasets();
			const expected2 = await insightFacade2.listDatasets();
			expect(expected1).to.deep.equal(expected2);
		});

		it("Should load room datasets on disk", async function () {
			const content: string = datasetContents.get("rooms") ?? "";
			await insightFacade.addDataset(id3, content, InsightDatasetKind.Rooms);
			const insightFacade2 = new InsightFacade();
			const expected1 = await insightFacade.listDatasets();
			const expected2 = await insightFacade2.listDatasets();
			expect(expected1).to.deep.equal(expected2);
		});

		it("Should load section and room datasets on disk", async function () {
			const content: string = datasetContents.get("sections") ?? "";
			const content2: string = datasetContents.get("rooms") ?? "";
			await insightFacade.addDataset(id, content, InsightDatasetKind.Sections);
			await insightFacade.addDataset(id3, content2, InsightDatasetKind.Rooms);
			const insightFacade2 = new InsightFacade();
			const expected1 = await insightFacade.listDatasets();
			const expected2 = await insightFacade2.listDatasets();
			expect(expected1).to.deep.equal(expected2);
		});

		it("should list no section datasets", function () {
			return insightFacade.listDatasets().then((insightDatasets) => {
				expect(insightDatasets).to.be.an.instanceof(Array);
				expect(insightDatasets).to.have.length(0);
			});
		});

		it("should list one section dataset", function () {
			const content: string = datasetContents.get("sections") ?? "";
			return insightFacade
				.addDataset("sections", content, InsightDatasetKind.Sections)
				.then(() => {
					return insightFacade.listDatasets();
				})
				.then((insightDatasets) => {
					expect(insightDatasets).to.deep.equal([
						{
							id: "sections",
							kind: InsightDatasetKind.Sections,
							numRows: 64612,
						},
					]);
				});
		});

		it("should list one room dataset", function () {
			const content: string = datasetContents.get("rooms") ?? "";
			return insightFacade
				.addDataset("rooms", content, InsightDatasetKind.Rooms)
				.then(() => {
					return insightFacade.listDatasets();
				})
				.then((insightDatasets) => {
					expect(insightDatasets).to.deep.equal([
						{
							id: "rooms",
							kind: InsightDatasetKind.Rooms,
							numRows: 364,
						},
					]);
				});
		});

		it("Should list multiple section datasets", async function () {
			const {expected1, expected2} = await addMultipleSectionDatasets();
			await assertMultipleDatasets(expected1, expected2);
		});

		it("Should list multiple room datasets", async function () {
			const {expected1, expected2} = await addMultipleRoomDatasets();
			await assertMultipleDatasets(expected1, expected2);
		});

		it("Should list multiple types of datasets", async function () {
			const content: string = datasetContents.get("sections") ?? "";
			const content2: string = datasetContents.get("rooms") ?? "";
			return insightFacade
				.addDataset("sections", content, InsightDatasetKind.Sections)
				.then(() => {
					return insightFacade.addDataset("rooms", content2, InsightDatasetKind.Rooms);
				})
				.then(() => {
					return insightFacade.listDatasets();
				})
				.then((insightDatasets) => {
					expect(insightDatasets).to.deep.equals([
						{
							id: "sections",
							kind: InsightDatasetKind.Sections,
							numRows: 64612,
						},
						{
							id: "rooms",
							kind: InsightDatasetKind.Rooms,
							numRows: 364,
						},
					]);
				});
		});

		it("should remove only one section dataset successfully", function () {
			const content: string = datasetContents.get("sections") ?? "";
			return insightFacade
				.addDataset("sections", content, InsightDatasetKind.Sections)
				.then(() => {
					return insightFacade.removeDataset("sections");
				})
				.then((insightDatasets) => {
					expect(insightDatasets).to.be.a("string");
					expect(insightDatasets).to.deep.equal("sections");
				});
		});

		it("should remove only one room dataset successfully", function () {
			const content: string = datasetContents.get("rooms") ?? "";
			return insightFacade
				.addDataset("rooms", content, InsightDatasetKind.Rooms)
				.then(() => {
					return insightFacade.removeDataset("rooms");
				})
				.then((insightDatasets) => {
					expect(insightDatasets).to.be.a("string");
					expect(insightDatasets).to.deep.equal("rooms");
				});
		});

		it("should remove one section dataset out of two datasets successfully", function () {
			const content: string = datasetContents.get("sections") ?? "";
			return insightFacade
				.addDataset("sections", content, InsightDatasetKind.Sections)
				.then(() => {
					return insightFacade.addDataset("sections-2", content, InsightDatasetKind.Sections);
				})
				.then(() => {
					return insightFacade.removeDataset("sections");
				})
				.then((insightDatasets) => {
					expect(insightDatasets).to.be.a("string");
					expect(insightDatasets).to.deep.equal("sections");
				});
		});

		it("should remove one room dataset out of two datasets successfully", function () {
			const content: string = datasetContents.get("rooms") ?? "";
			return insightFacade
				.addDataset("rooms", content, InsightDatasetKind.Rooms)
				.then(() => {
					return insightFacade.addDataset("rooms-2", content, InsightDatasetKind.Rooms);
				})
				.then(() => {
					return insightFacade.removeDataset("rooms");
				})
				.then((insightDatasets) => {
					expect(insightDatasets).to.be.a("string");
					expect(insightDatasets).to.deep.equal("rooms");
				});
		});

		it("Should reject when dataset cannot be found", async function () {
			try {
				await insightFacade.removeDataset(id);
				expect.fail("Should not have resolved");
			} catch (err) {
				expect(err).to.be.instanceof(NotFoundError);
			}
		});

		it("Should reject when given a dataset id with underscores", async function () {
			const invalidId: string = "sections_invalid";
			try {
				await insightFacade.removeDataset(invalidId);
				expect.fail("Should not have resolved");
			} catch (err) {
				expect(err).to.be.instanceof(InsightError);
			}
		});

		it("Should reject when given a dataset id with only whitespaces", async function () {
			const invalidId: string = " ";
			try {
				await insightFacade.removeDataset(invalidId);
				expect.fail("Should not have resolved");
			} catch (err) {
				expect(err).to.be.instanceof(InsightError);
			}
		});
	});

	/*
	 * This test suite dynamically generates tests from the JSON files in test/resources/queries.
	 * You should not need to modify it; instead, add additional files to the queries directory.
	 * You can still make tests the normal way, this is just a convenient tool for a majority of queries.
	 */
	describe("performQuery", () => {
		before(function () {
			console.info(`Before: ${this.test?.parent?.title}`);

			insightFacade = new InsightFacade();

			// Load the datasets specified in datasetsToQuery and add them to InsightFacade.
			// Will *fail* if there is a problem reading ANY dataset.
			const loadDatasetPromises = [
				insightFacade.addDataset(
					"sections",
					datasetContents.get("sections") ?? "",
					InsightDatasetKind.Sections
				),
			];

			return Promise.all(loadDatasetPromises);
		});

		after(function () {
			console.info(`After: ${this.test?.parent?.title}`);
			fs.removeSync(persistDirectory);
		});

		type PQErrorKind = "ResultTooLargeError" | "InsightError";

		folderTest<unknown, Promise<InsightResult[]>, PQErrorKind>(
			"Dynamic InsightFacade performQuery tests",
			(input) => insightFacade.performQuery(input),
			"./test/resources/queries",
			{
				assertOnResult: (actual, expected) => {
					expect(actual).to.have.deep.members(expected);
				},
				errorValidator: (error): error is PQErrorKind =>
					error === "ResultTooLargeError" || error === "InsightError",
				assertOnError: (actual, expected) => {
					if (expected === "ResultTooLargeError") {
						expect(actual).to.be.instanceof(ResultTooLargeError);
					} else {
						expect(actual).to.be.instanceof(InsightError);
					}
				},
			}
		);
	});
});
