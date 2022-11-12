import {Query, Where} from "../model/Query";
import {Section} from "../model/dataset/Section";
import {SectionDataset} from "../model/dataset/SectionDataset";
import {ValidateQueryFunctions} from "./ValidateQueryFunctions";
import {InsightError, InsightResult, ResultTooLargeError} from "./IInsightFacade";

export class PerformQueryFunctions {
	public static performWHERE(query: Query, sectionDataset: SectionDataset): Section[] {
		const result = this.performFilter(query, query.WHERE, sectionDataset.getSections());
		if (result.length > 5000) {
			throw new ResultTooLargeError("Result size is over 5000");
		} else {
			return result;
		}
	}

	public static getQueriedSectionDataset(q: Query, datasets: SectionDataset[]): SectionDataset {
		let queriedDatasetId: string;
		queriedDatasetId = ValidateQueryFunctions.getDatasetId(q);

		for (let sectionDataset of datasets) {
			if (sectionDataset.getId() === queriedDatasetId) {
				return sectionDataset;
			}
		}
		throw new InsightError("Dataset Not Found.");
	}

	public static performFilter(q: Query, filter: Where, sections: Section[]): Section[] {
		if (filter.AND !== undefined) {
			if (filter.AND.length > 1) {
				let resultSoFar: Section[] = this.performFilter(q, filter.AND[0], sections);
				for (let i = 1; i < filter.AND.length; i++) {
					resultSoFar = this.performFilter(q, filter.AND[i], resultSoFar);
				}
				return resultSoFar;
			} else {
				return this.performFilter(q, filter.AND[0], sections);
			}
		} else if (filter.OR !== undefined) {
			if (filter.OR.length > 1) {
				let resultSoFar: Section[] = this.performFilter(q, filter.OR[0], sections);
				for (let i = 1; i < filter.OR.length; i++) {
					resultSoFar = Array.from(
						new Set([...resultSoFar, ...this.performFilter(q, filter.OR[i], sections)])
					);
				}
				return resultSoFar;
			} else {
				return this.performFilter(q, filter.OR[0], sections);
			}
		} else if (filter.LT !== undefined) {
			const ltFunc = (x: number, y: number) => x < y;
			return this.performMComparison(filter.LT, ltFunc, sections);
		} else if (filter.GT !== undefined) {
			const gtFunc = (x: number, y: number) => x > y;
			return this.performMComparison(filter.GT, gtFunc, sections);
		} else if (filter.EQ !== undefined) {
			const eqFunc = (x: number, y: number) => x === y;
			return this.performMComparison(filter.EQ, eqFunc, sections);
		} else if (filter.IS !== undefined) {
			const isFunc = this.wildCardHelper();
			return this.performSComparison(filter.IS, isFunc, sections);
		} else if (filter.NOT !== undefined) {
			const deleteArr = this.performFilter(q, filter.NOT, sections);
			const deleteSet = new Set(deleteArr);
			return sections.filter((section) => {
				return !deleteSet.has(section);
			});
		}
		return sections;
	}

	public static wildCardHelper() {
		return (name: string, criteria: string) => {
			let criteriaWithoutWildcards = criteria.replace(/\*/g, "");
			if (criteria.toString().charAt(0) === "*" && criteria.charAt(criteria.length - 1) === "*") {
				return name.toString().includes(criteriaWithoutWildcards);
			} else if (criteria.toString().charAt(0) === "*") {
				return name.toString().endsWith(criteriaWithoutWildcards);
			} else if (criteria.toString().charAt(criteria.length - 1) === "*") {
				return name.toString().startsWith(criteriaWithoutWildcards);
			} else {
				return name === criteria;
			}
		};
	}

	public static performMComparison(
		mComp: object,
		mCompFunc: (x: number, y: number) => boolean,
		sections: Section[]
	): Section[] {
		const mField = Object.keys(mComp)[Object.values(mComp).length - 1].split("_")[1];
		const threshold = Object.values(mComp)[Object.values(mComp).length - 1];
		if (mField === "avg") {
			return sections.filter((entry) => mCompFunc(entry.avg, threshold));
		} else if (mField === "pass") {
			return sections.filter((entry) => mCompFunc(entry.pass, threshold));
		} else if (mField === "fail") {
			return sections.filter((entry) => mCompFunc(entry.fail, threshold));
		} else if (mField === "audit") {
			return sections.filter((entry) => mCompFunc(entry.audit, threshold));
		} else {
			// mField === "year"
			return sections.filter((entry) => mCompFunc(entry.year, threshold));
		}
	}

	public static performSComparison(
		sComp: object,
		sCompFunc: (x: string, y: string) => boolean,
		sections: Section[]
	): Section[] {
		const sField = Object.keys(sComp)[Object.values(sComp).length - 1].split("_")[1];
		const criteria = Object.values(sComp)[Object.values(sComp).length - 1];
		if (sField === "dept") {
			return sections.filter((entry) => sCompFunc(entry.dept, criteria));
		} else if (sField === "id") {
			return sections.filter((entry) => sCompFunc(entry.id, criteria));
		} else if (sField === "instructor") {
			return sections.filter((entry) => sCompFunc(entry.instructor, criteria));
		} else if (sField === "title") {
			return sections.filter((entry) => sCompFunc(entry.title, criteria));
		} else {
			// sField === "uuid"
			return sections.filter((entry) => sCompFunc(entry.uuid.toString(), criteria));
		}
	}

	public static performOPTIONS(q: Query, filteredSections: Section[]): InsightResult[] {
		const cols = this.performCOLUMNS(q, filteredSections);
		return this.performORDER(q, cols);
	}

	public static performCOLUMNS(q: Query, filteredSections: Section[]): InsightResult[] {
		let sectionCols: InsightResult[] = [];
		for (let i = 0; i < filteredSections.length; i++) {
			sectionCols[i] = {};
			for (const item of q.OPTIONS.COLUMNS) {
				let itemField = item.split("_")[1];
				type SectionKey = keyof InsightResult;
				const key = item as SectionKey;
				sectionCols[i][key] = filteredSections[i].getProperty(itemField);
			}
		}
		return sectionCols;
	}

	public static performORDER(q: Query, cols: InsightResult[]): InsightResult[] {
		if (q.OPTIONS.ORDER === undefined) {
			return cols;
		} else if (
			q.OPTIONS.ORDER === ValidateQueryFunctions.getDatasetId(q) + "_" + "avg" ||
			q.OPTIONS.ORDER === ValidateQueryFunctions.getDatasetId(q) + "_" + "pass" ||
			q.OPTIONS.ORDER === ValidateQueryFunctions.getDatasetId(q) + "_" + "fail" ||
			q.OPTIONS.ORDER === ValidateQueryFunctions.getDatasetId(q) + "_" + "audit" ||
			q.OPTIONS.ORDER === ValidateQueryFunctions.getDatasetId(q) + "_" + "year"
		) {
			type ObjectKey = keyof InsightResult;
			const orderKey = q.OPTIONS.ORDER as ObjectKey;
			return cols.sort((e1, e2) => (e1[orderKey] as number) - (e2[orderKey] as number));
		} else {
			type ObjectKey = keyof InsightResult;
			const orderKey = q.OPTIONS.ORDER as ObjectKey;
			return cols.sort((e1, e2) => {
				if (e1[orderKey] > e2[orderKey]) {
					return 1;
				} else if (e1[orderKey] < e2[orderKey]) {
					return -1;
				} else {
					return 0;
				}
			});
		}
	}
}
