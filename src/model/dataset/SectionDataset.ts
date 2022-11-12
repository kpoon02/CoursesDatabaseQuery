import {InsightDataset, InsightDatasetKind} from "../../controller/IInsightFacade";
import {Section} from "./Section";

export class SectionDataset {
	private readonly datasetInfo: InsightDataset;
	private readonly sections: Section[];

	constructor(id: string, numRows: number, sections: Section[]) {
		this.datasetInfo = {
			id: id,
			kind: InsightDatasetKind.Sections,
			numRows: numRows,
		};
		this.sections = sections;
	}

	public getId(): string {
		return this.datasetInfo.id;
	}

	public getInsightDataset(): InsightDataset {
		return this.datasetInfo;
	}

	public getSections(): Section[] {
		return this.sections;
	}
}
