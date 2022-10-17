import {InsightDataset, InsightDatasetKind} from "../controller/IInsightFacade";
import {Section} from "./Section";

export class SectionDataset {
	private datasetInfo: InsightDataset;
	private sections: Section[];

	constructor(id: string, kind: InsightDatasetKind, numRows: number, sections: Section[]) {
		this.datasetInfo = {
			id: id,
			kind: kind,
			numRows: numRows,
		};
		this.sections = sections;
	}

	public getId(): string {
		return this.datasetInfo.id;
	}

	public getNumRows(): number {
		return this.datasetInfo.numRows;
	}

	public getInsightDataset(): InsightDataset {
		return this.datasetInfo;
	}

	public getSections(): Section[] {
		return this.sections;
	}
}
