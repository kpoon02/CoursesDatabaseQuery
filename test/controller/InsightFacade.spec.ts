import {
	IInsightFacade,
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
		sections: "./test/resources/archives/pair.zip",
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
		const content: string = datasetContents.get("sections") ?? "";

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

		it("Should add a valid dataset", async function () {
			const expected: string[] = [id];
			const result = await insightFacade.addDataset(id, content, InsightDatasetKind.Sections);

			expect(result).to.deep.equal(expected);
		});

		it("Should add multiple valid datasets", async function () {
			const id2: string = "sections2";
			const content2: string = datasetContents.get("sections") ?? "";
			const expected: string[] = [id, id2];
			await insightFacade.addDataset(id, content, InsightDatasetKind.Sections);
			const result = await insightFacade.addDataset(id2, content2, InsightDatasetKind.Sections);

			expect(result).to.deep.equal(expected);
		});

		it("Should reject datasets with underscores in id", async function () {
			const invalidId: string = "sections_invalid";
			try {
				await insightFacade.addDataset(invalidId, content, InsightDatasetKind.Sections);
				expect.fail("Should not have resolved");
			} catch (err) {
				expect(err).to.be.instanceof(InsightError);
			}
		});

		it("Should reject datasets with only whitespaces in id", async function () {
			const invalidId: string = " ";
			try {
				await insightFacade.addDataset(invalidId, content, InsightDatasetKind.Sections);
				expect.fail("Should not have resolved");
			} catch (err) {
				expect(err).to.be.instanceof(InsightError);
			}
		});

		it("Should reject datasets already on disk", async function () {
			await insightFacade.addDataset(id, content, InsightDatasetKind.Sections);
			try {
				await insightFacade.addDataset(id, content, InsightDatasetKind.Sections);
				expect.fail("Should not have resolved");
			} catch (err) {
				expect(err).to.be.instanceof(InsightError);
			}
		});

		it("Should list multiple datasets", async function () {
			const id2: string = "sections2";
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
			const result = await insightFacade.listDatasets();

			expect(result).to.be.instanceof(Array);
			expect(result).to.have.length(2);
			expect(result).to.deep.include(expected1);
			expect(result).to.deep.include(expected2);
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

		// Katherine's tests
		// list datasets tests
		describe("List Datasets", function () {
			it("should list no datasets", function () {
				return insightFacade.listDatasets().then((insightDatasets) => {
					expect(insightDatasets).to.be.an.instanceof(Array);
					expect(insightDatasets).to.have.length(0);
				});
			});

			it("should list one dataset", function () {
				return insightFacade
					.addDataset("sections", content, InsightDatasetKind.Sections)
					.then((addedIds) => {
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
		});

		// remove dataset tests
		describe("Remove Datasets", function () {
			let facade: IInsightFacade;

			beforeEach(function () {
				facade = new InsightFacade();
			});

			it("should remove only dataset successfully", function () {
				return facade
					.addDataset("sections", content, InsightDatasetKind.Sections)
					.then(() => {
						return facade.removeDataset("sections");
					})
					.then((insightDatasets) => {
						expect(insightDatasets).to.be.a("string");
						expect(insightDatasets).to.deep.equal("sections");
					});
			});

			it("should remove dataset out of two datasets successfully", function () {
				return facade
					.addDataset("sections", content, InsightDatasetKind.Sections)
					.then(() => {
						return facade.addDataset("sections-2", content, InsightDatasetKind.Sections);
					})
					.then(() => {
						return facade.removeDataset("sections");
					})
					.then((insightDatasets) => {
						expect(insightDatasets).to.be.a("string");
						expect(insightDatasets).to.deep.equal("sections");
					});
			});
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
