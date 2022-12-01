import {IInsightFacade, InsightDataset, InsightDatasetKind, InsightError, InsightResult} from "./IInsightFacade";
import {ValidateQueryFunctions} from "./ValidateQueryFunctions";
import {Query} from "../model/Query";
import {PerformQueryWhereFunctions} from "./PerformQueryWhereFunctions";
import {PerformQueryTransformationsFunctions} from "./PerformQueryTransformationsFunctions";
import {DatasetController} from "./dataset/DatasetController";
import {PerformQueryOptionsFunctions} from "./PerformQueryOptionsFunctions";
import * as fs from "fs-extra";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */
export default class InsightFacade implements IInsightFacade {
	private datasetController: DatasetController;

	constructor() {
		console.log("InsightFacadeImpl::init()");
		this.datasetController = new DatasetController();
	}

	public async addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
		return Promise.resolve(this.datasetController.addDataset(id, content, kind));
	}

	public removeDataset(id: string): Promise<string> {
		return Promise.resolve(this.datasetController.removeDataset(id));
	}

	// don't think this works if this is called before adding a valid dataset
	public saveQuery(query: Query) {
		const dir = "./data/history";
		let history: Query[] = [];
		if (!fs.existsSync(dir)) {
			fs.mkdirSync(dir);
		}
		if (fs.existsSync(dir + "/history.json")) {
			history = JSON.parse(fs.readFileSync(dir + "/history.json").toString());
			if (history.length === 5) {
				history[0] = history[1];
				history[1] = history[2];
				history[2] = history[3];
				history[3] = history[4];
				history[4] = query;
			} else {
				history.push(query);
			}
			fs.writeFileSync(dir + "/history.json", JSON.stringify(history));
		} else {
			history.push(query);
			fs.writeFileSync(dir + "/history.json", JSON.stringify(history));
		}
	}

	// don't think this works if this is called before adding a valid dataset
	public getHistory(): Query[] {
		const dir = "./data/history";
		if (!fs.existsSync(dir)) {
			fs.mkdirSync(dir);
		}
		if (!fs.existsSync(dir + "/history.json")) {
			fs.writeFileSync(dir + "/history.json", JSON.stringify([]));
		}
		return JSON.parse(fs.readFileSync(dir + "/history.json").toString());
	}

	public performQuery(query: unknown): Promise<InsightResult[]> {
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
			let queryType: string = "";
			let datasets = [];
			if (ValidateQueryFunctions.validateQuery(query, this.datasetController.getSectionDatasets(), "section")) {
				queryType = "section";
				datasets = this.datasetController.getSectionDatasets();
			} else if (ValidateQueryFunctions.validateQuery(query, this.datasetController.getRoomDatasets(), "room")) {
				queryType = "room";
				datasets = this.datasetController.getRoomDatasets();
			} else {
				return Promise.reject(new InsightError("Not a valid query."));
			}
			return Promise.resolve(
				PerformQueryOptionsFunctions.performOPTIONS(
					query,
					PerformQueryTransformationsFunctions.performTRANSFORMATIONS(
						query,
						PerformQueryWhereFunctions.performWHERE(
							query,
							PerformQueryWhereFunctions.getQueriedDataset(query, datasets),
							queryType
						),
						queryType
					),
					queryType
				)
			);
		} else {
			return Promise.reject(new InsightError("Not a query."));
		}
	}

	public listDatasets(): Promise<InsightDataset[]> {
		return Promise.resolve(this.datasetController.listDataset());
	}
}
