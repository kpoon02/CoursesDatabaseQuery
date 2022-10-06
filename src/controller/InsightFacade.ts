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

interface JsonCourse {
	result: JsonSection[];
}

export interface JsonSection {
	Title: string;
	id: string; // Section UUID
	Professor: string; // Section INSTRUCTOR
	Audit: number;
	Year: number;
	Course: string; // Section ID
	Pass: number;
	Fail: number;
	Avg: number;
	Subject: string; // Section DEPT
}

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */
export default class InsightFacade implements IInsightFacade {
	private readonly datasets: SectionDataset[];

	constructor() {
		this.datasets = [];
		console.log("InsightFacadeImpl::init()");
	}

	public async addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
		let sections: Section[] = [];
		let promises: Array<Promise<string>> = [];
		const ids: string[] = [];

		this.validateDatasetId(id);

		// load the zip file into jszip
		let jszip: JSZip | null = await JSZip.loadAsync(content, {base64: true});
		// throw InsightError if the zip file does not have a 'courses' root folder, otherwise load each file
		jszip = jszip.folder("courses");
		if (jszip == null) {
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
		this.saveToDisk(sections, id);
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

	// parses the json string and returns an array of Courses. Only adds sections with queryable data
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
		if (section.Subject == null || section.Subject === "") {
			return false;
		}
		if (section.id == null || section.id === "") {
			return false;
		}
		if (section.Avg == null) {
			return false;
		}
		if (section.Professor == null || section.Professor === "") {
			return false;
		}
		if (section.Title == null || section.Title === "") {
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
		if (section.Course == null || section.Course === "") {
			return false;
		}
		return section.Year != null;
	}

	// checks if the data directory exists, if not create it. Then add the dataset to data.
	public saveToDisk(courses: Section[], id: string) {
		const data = JSON.stringify(courses);
		const dir = "./data";
		if (!fs.existsSync(dir)) {
			fs.mkdirSync(dir);
		}
		fs.writeFileSync(dir + "/" + id + ".json", data);
	}

	public removeDataset(id: string): Promise<string> {
		return Promise.reject("Not implemented.");
	}

	public performQuery(query: unknown): Promise<InsightResult[]> {
		return Promise.reject("Not implemented.");
	}

	public listDatasets(): Promise<InsightDataset[]> {
		return Promise.reject("Not implemented.");
	}
}
