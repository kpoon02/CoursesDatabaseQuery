import {InsightDataset, InsightDatasetKind, InsightError, NotFoundError} from "../IInsightFacade";
import * as fs from "fs-extra";
import {SectionDataset} from "../../model/dataset/SectionDataset";
import {RoomDataset} from "../../model/dataset/RoomDataset";
import {SectionDatasetController} from "./SectionDatasetController";
import {RoomDatasetController} from "./RoomDatasetController";

export class DatasetController {
	private sectionDatasetController: SectionDatasetController;
	private roomDatasetController: RoomDatasetController;

	constructor() {
		this.sectionDatasetController = new SectionDatasetController();
		this.roomDatasetController = new RoomDatasetController();
		this.sectionDatasetController.loadDatasetsFromDisk();
		this.roomDatasetController.loadDatasetsFromDisk();
	}

	public async addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
		const ids: string[] = [];

		this.validateDatasetId(id);
		if (this.doesDatasetIdAlreadyExist(id)) {
			throw new InsightError("Cannot add new dataset with an already existing ID");
		}

		if (kind === InsightDatasetKind.Sections) {
			await this.sectionDatasetController.addDataset(id, content);
			for (let dataset of this.sectionDatasetController.getDatasets()) {
				ids.push(dataset.getId());
			}
		} else if (kind === InsightDatasetKind.Rooms) {
			await this.roomDatasetController.addDataset(id, content);
			for (let dataset of this.roomDatasetController.getDatasets()) {
				ids.push(dataset.getId());
			}
		} else {
			throw new InsightError("Invalid InsightDatasetKind type");
		}
		return ids;
	}

	// throws an InsightError if the dataset id contains an underscore, only spaces, or is a duplicate
	public validateDatasetId(id: string) {
		if (id.includes("_")) {
			throw new InsightError("Dataset IDs cannot contain underscores");
		}
		const regex: RegExp = /^\s+$/;
		if (regex.test(id)) {
			throw new InsightError("Dataset IDs cannot consist of only white space");
		}
	}

	public doesDatasetIdAlreadyExist(id: string): boolean {
		if (fs.existsSync("./data")) {
			if (fs.existsSync("./data/section")) {
				const existingDatasets = fs.readdirSync("./data/section");
				for (let s of existingDatasets) {
					if (s === id + ".json") {
						return true;
					}
				}
			}
			if (fs.existsSync("./data/room")) {
				const existingDatasets = fs.readdirSync("./data/room");
				for (let s of existingDatasets) {
					if (s === id + ".json") {
						return true;
					}
				}
			}
		}
		return false;
	}

	// REQUIRES: dir to be a valid path to the data directory
	// EFFECTS: checks if the data directory exists, if not create it. Then add the dataset to data.
	public static saveToDisk(dataset: SectionDataset | RoomDataset, id: string, dir: string) {
		const data = JSON.stringify(dataset);
		if (!fs.existsSync(dir)) {
			fs.mkdirSync("./data");
			fs.mkdirSync(dir);
		}
		fs.writeFileSync(dir + "/" + id + ".json", data);
	}

	public removeDataset(id: string): string {
		this.validateDatasetId(id);
		if (fs.existsSync("./data/section")) {
			const files = fs.readdirSync("./data/section");
			for (const file of files) {
				if (file === id + ".json") {
					fs.removeSync("./data/section/" + file);
					return id;
				}
			}
		}
		if (fs.existsSync("./data/room")) {
			const files = fs.readdirSync("./data/room");
			for (const file of files) {
				if (file === id + ".json") {
					fs.removeSync("./data/room/" + file);
					return id;
				}
			}
		}
		throw new NotFoundError("Dataset ID not found");
	}

	public listDataset(): InsightDataset[] {
		const result: InsightDataset[] = [];
		for (const dataset of this.sectionDatasetController.getDatasets()) {
			result.push(dataset.getInsightDataset());
		}
		for (const dataset of this.roomDatasetController.getDatasets()) {
			result.push(dataset.getInsightDataset());
		}
		return result;
	}

	public getSectionDatasets() {
		return this.sectionDatasetController.getDatasets();
	}

	public getRoomDatasets() {
		return this.roomDatasetController.getDatasets();
	}
}
