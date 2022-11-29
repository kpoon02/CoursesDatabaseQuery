import * as fs from "fs-extra";
import {SectionDataset} from "../../model/dataset/SectionDataset";
import {Section} from "../../model/dataset/Section";
import {InsightDataset, InsightError} from "../IInsightFacade";
import JSZip from "jszip";
import {DatasetController} from "./DatasetController";

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

const DIR = "./data/section";

export class SectionDatasetController {
	private readonly datasets: SectionDataset[];

	constructor() {
		console.log("SectionDatasetController::init()");
		this.datasets = [];
	}

	public loadDatasetsFromDisk() {
		if (fs.existsSync(DIR)) {
			const files = fs.readdirSync(DIR);
			for (const file of files) {
				const content = JSON.parse(fs.readFileSync(DIR + "/" + file).toString());
				this.datasets.push(this.parseDatasetFromDisk(content));
			}
		}
	}

	public async addDataset(id: string, content: string) {
		let sections: Section[] = [];
		let promises: Array<Promise<string>> = [];

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

		const sectionDataset = new SectionDataset(id, sections.length, sections);
		this.datasets.push(sectionDataset);
		DatasetController.saveToDisk(sectionDataset, id, DIR);
	}

	// parses the json object and returns a SectionDataset
	private parseDatasetFromDisk(json: object): SectionDataset {
		const sections: Section[] = [];
		const results = json as unknown as JsonDataset;
		const datasetInfo = results.datasetInfo as InsightDataset;
		for (let section of results.sections) {
			sections.push(new Section(section).fixProperties(section));
		}
		return new SectionDataset(datasetInfo.id, datasetInfo.numRows, sections);
	}

	// parses the json string and returns an array of Section. Only adds sections with queryable data
	private parseCourseData(json: string): Section[] {
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
	private isQueryableSection(section: JsonSection): boolean {
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

	public getDatasets(): SectionDataset[] {
		return this.datasets;
	}
}
