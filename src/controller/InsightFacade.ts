import {
	IInsightFacade,
	InsightDataset,
	InsightDatasetKind,
	InsightError,
	InsightResult,
	NotFoundError,
} from "./IInsightFacade";
import {SectionDataset} from "../model/SectionDataset";
import JSZip from "jszip";
import * as fs from "fs-extra";
import {Section} from "../model/Section";
import {ValidateQueryFunctions} from "./ValidateQueryFunctions";
import {Query} from "../model/Query";
import {PerformQueryFunctions} from "./PerformQueryFunctions";

interface JsonCourse {
	result: JsonSection[];
}

export interface JsonSection {
	Title: string;
	id: string; // Section UUID
	Professor: string; // Section INSTRUCTOR
	Audit: number;
	Year: string;
	Course: string; // Section ID
	Pass: number;
	Fail: number;
	Avg: number;
	Subject: string; // Section DEPT
	Section: string;
}

interface JsonDataset {
	datasetInfo: InsightDataset;
	sections: JsonSection[];
}

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */
export default class InsightFacade implements IInsightFacade {
	private readonly datasets: SectionDataset[];

	constructor() {
		console.log("InsightFacadeImpl::init()");
		this.datasets = [];
		this.loadDatasetsFromDisk();
	}

	public loadDatasetsFromDisk() {
		const dir = "./data";
		if (fs.existsSync(dir)) {
			const files = fs.readdirSync(dir);
			for (const file of files) {
				const content = JSON.parse(fs.readFileSync(dir + "/" + file).toString());
				this.datasets.push(this.parseDatasetFromDisk(content));
			}
		}
	}

	public async addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
		let sections: Section[] = [];
		let promises: Array<Promise<string>> = [];
		const ids: string[] = [];

		this.validateDatasetId(id);

		// load the zip file into jszip
		let jszip: JSZip | null = await JSZip.loadAsync(content, {base64: true});
		// throw InsightError if the zip file does not have a 'courses' root folder, otherwise load each file
		const numRootFolders = jszip.folder(/^courses\/$/).length;
		jszip = jszip.folder("courses");
		if (jszip == null || numRootFolders === 0) {
			throw new InsightError("The dataset zip file does not contain a courses root folder");
		} else {
			jszip.forEach((relativePath, file) => {
				promises.push(file.async("string"));
			});
		}

		// wait for all promises to resolve, parse file data into Section objects
		await Promise.all(promises)
			.then((results) => {
				for (let course of results) {
					const courseSections: Section[] = this.parseCourseData(course);
					sections = sections.concat(courseSections);
				}
			})
			.catch((err) => {
				throw new InsightError(err);
			});

		// add the dataset to memory and disk then return dataset ids
		const sectionDataset = new SectionDataset(id, kind, sections.length, sections);
		this.datasets.push(sectionDataset);
		this.saveToDisk(sectionDataset, id);
		for (let dataset of this.datasets) {
			ids.push(dataset.getId());
		}
		return Promise.resolve(ids);
	}

	// throws an InsightError if the dataset id contains an underscore, only spaces, or is a duplicate
	private validateDatasetId(id: string) {
		if (id.includes("_")) {
			throw new InsightError("Dataset IDs cannot contain underscores");
		}
		const regex: RegExp = /^\s+$/;
		if (regex.test(id)) {
			throw new InsightError("Dataset IDs cannot consist of only white space");
		}
		const dir = "./data";
		if (fs.existsSync(dir)) {
			const existingDatasets = fs.readdirSync(dir);
			for (let s of existingDatasets) {
				if (s === id + ".json") {
					throw new InsightError("Cannot add new dataset with an already existing ID");
				}
			}
		}
	}

	// parses the json object and returns a SectionDataset
	public parseDatasetFromDisk(json: object): SectionDataset {
		const sections: Section[] = [];
		const results = json as unknown as JsonDataset;
		const datasetInfo = results.datasetInfo;
		for (let section of results.sections) {
			sections.push(new Section(section));
		}
		return new SectionDataset(datasetInfo.id, datasetInfo.kind, datasetInfo.numRows, sections);
	}

	// parses the json string and returns an array of Section. Only adds sections with queryable data
	public parseCourseData(json: string): Section[] {
		const returnValue: Section[] = [];
		const results: JsonCourse = JSON.parse(json);
		const sections: JsonSection[] = results.result;

		for (let section of sections) {
			if (this.isQueryableSection(section)) {
				returnValue.push(new Section(section));
			}
		}
		return returnValue;
	}

	// if any queryable field is not present, return false
	public isQueryableSection(section: JsonSection): boolean {
		if (section.Subject == null) {
			return false;
		}
		if (section.id == null) {
			return false;
		}
		if (section.Avg == null) {
			return false;
		}
		if (section.Professor == null) {
			return false;
		}
		if (section.Title == null) {
			return false;
		}
		if (section.Pass == null) {
			return false;
		}
		if (section.Fail == null) {
			return false;
		}
		if (section.Audit == null) {
			return false;
		}
		if (section.Course == null) {
			return false;
		}
		return section.Year != null;
	}

	// checks if the data directory exists, if not create it. Then add the dataset to data.
	public saveToDisk(dataset: SectionDataset, id: string) {
		const data = JSON.stringify(dataset);
		const dir = "./data";
		if (!fs.existsSync(dir)) {
			fs.mkdirSync(dir);
		}
		fs.writeFileSync(dir + "/" + id + ".json", data);
	}

	public removeDataset(id: string): Promise<string> {
		if (id.includes("_")) {
			throw new InsightError("Dataset IDs cannot contain underscores");
		}
		const regex: RegExp = /^\s+$/;
		if (regex.test(id)) {
			throw new InsightError("Dataset IDs cannot consist of only white space");
		}
		const dir = "./data";
		if (fs.existsSync(dir)) {
			const files = fs.readdirSync(dir);
			for (const file of files) {
				if (file === id + ".json") {
					fs.removeSync(dir + "/" + file);
					return Promise.resolve(id);
				}
			}
		}
		throw new NotFoundError("Dataset ID not found");
	}

	public performQuery(query: unknown): Promise<InsightResult[]> {
		// using type predicates to ensure input query is of type Query
		function isQuery(q: unknown): q is Query {
			return (
				q !== null &&
				q !== undefined &&
				typeof q === "object" &&
				(q as Query).WHERE !== undefined &&
				(q as Query).OPTIONS !== undefined &&
				(q as Query).OPTIONS.COLUMNS !== undefined
			);
		}

		if (isQuery(query)) {
			if (ValidateQueryFunctions.validateQuery(query, this.datasets)) {
				return Promise.resolve(
					PerformQueryFunctions.performOPTIONS(
						query,
						PerformQueryFunctions.performWHERE(
							query,
							PerformQueryFunctions.getQueriedSectionDataset(query, this.datasets)
						)
					)
				);
			} else {
				return Promise.reject(new InsightError("Not a valid query."));
			}
		} else {
			return Promise.reject(new InsightError("Not a query."));
		}
	}

	public listDatasets(): Promise<InsightDataset[]> {
		const result: InsightDataset[] = [];
		for (const dataset of this.datasets) {
			result.push(dataset.getInsightDataset());
		}
		return Promise.resolve(result);
	}
}
