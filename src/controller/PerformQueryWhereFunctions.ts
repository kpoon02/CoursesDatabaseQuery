import {Query, Where} from "../model/Query";
import {Section} from "../model/dataset/Section";
import {SectionDataset} from "../model/dataset/SectionDataset";
import {ValidateQueryFunctions} from "./ValidateQueryFunctions";
import {InsightError, ResultTooLargeError} from "./IInsightFacade";
import {RoomDataset} from "../model/dataset/RoomDataset";
import {Room} from "../model/dataset/Room";

export class PerformQueryWhereFunctions {
	public static performWHERE(
		query: Query,
		dataset: SectionDataset | RoomDataset,
		queryType: string
	): Section[] | Room[] {
		let result;
		if (queryType === "section") {
			dataset = dataset as SectionDataset;
			result = this.performFilter(query, query.WHERE, dataset.getSections(), queryType);
		} else {
			dataset = dataset as RoomDataset;
			result = this.performFilter(query, query.WHERE, dataset.getRooms(), queryType);
		}
		if (query.TRANSFORMATIONS === undefined && result.length > 5000) {
			throw new ResultTooLargeError("Result size is over 5000");
		} else {
			return result;
		}
	}

	public static getQueriedDataset(
		q: Query,
		datasets: SectionDataset[] | RoomDataset[]
	): SectionDataset | RoomDataset {
		let queriedDatasetId: string;
		queriedDatasetId = ValidateQueryFunctions.getDatasetId(q);

		for (let dataset of datasets) {
			if (dataset.getId() === queriedDatasetId) {
				return dataset;
			}
		}
		throw new InsightError("Dataset Not Found.");
	}

	public static performFilter(
		q: Query,
		filter: Where,
		entries: Section[] | Room[],
		queryType: string
	): Section[] | Room[] {
		if (filter.AND !== undefined) {
			return this.performAND(filter, q, entries, queryType);
		} else if (filter.OR !== undefined) {
			return this.performOR(filter, q, entries, queryType);
		} else if (filter.LT !== undefined) {
			const ltFunc = (x: number, y: number) => x < y;
			return this.performMComparison(filter.LT, ltFunc, entries, queryType);
		} else if (filter.GT !== undefined) {
			const gtFunc = (x: number, y: number) => x > y;
			return this.performMComparison(filter.GT, gtFunc, entries, queryType);
		} else if (filter.EQ !== undefined) {
			const eqFunc = (x: number, y: number) => x === y;
			return this.performMComparison(filter.EQ, eqFunc, entries, queryType);
		} else if (filter.IS !== undefined) {
			const isFunc = this.wildCardHelper();
			return this.performSComparison(filter.IS, isFunc, entries, queryType);
		} else if (filter.NOT !== undefined) {
			if (queryType === "section") {
				const deleteArr = this.performFilter(q, filter.NOT, entries, queryType) as Section[];
				const deleteSet = new Set(deleteArr);
				entries = entries as Section[];
				return entries.filter((entry) => {
					return !deleteSet.has(entry);
				});
			} else {
				const deleteArr = this.performFilter(q, filter.NOT, entries, queryType) as Room[];
				const deleteSet = new Set(deleteArr);
				entries = entries as Room[];
				return entries.filter((entry) => {
					return !deleteSet.has(entry);
				});
			}
		}
		return entries;
	}

	private static performAND(filter: Where, q: Query, entries: Section[] | Room[], queryType: string) {
		if (filter.AND === undefined) {
			return [];
		}
		if (filter.AND.length > 1) {
			let resultSoFar = this.performFilter(q, filter.AND[0], entries, queryType);
			for (let i = 1; i < filter.AND.length; i++) {
				resultSoFar = this.performFilter(q, filter.AND[i], resultSoFar, queryType);
			}
			return resultSoFar;
		} else {
			return this.performFilter(q, filter.AND[0], entries, queryType);
		}
	}

	private static performOR(filter: Where, q: Query, entries: Section[] | Room[], queryType: string) {
		if (filter.OR === undefined) {
			return [];
		}
		if (filter.OR.length > 1) {
			let resultSoFar = this.performFilter(q, filter.OR[0], entries, queryType);
			for (let i = 1; i < filter.OR.length; i++) {
				if (queryType === "section") {
					resultSoFar = Array.from(
						new Set([
							...(resultSoFar as Section[]),
							...(this.performFilter(q, filter.OR[i], entries, queryType) as Section[]),
						])
					);
				} else {
					resultSoFar = Array.from(
						new Set([
							...(resultSoFar as Room[]),
							...(this.performFilter(q, filter.OR[i], entries, queryType) as Room[]),
						])
					);
				}
			}
			return resultSoFar;
		} else {
			return this.performFilter(q, filter.OR[0], entries, queryType);
		}
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
		entries: Section[] | Room[],
		queryType: string
	): Section[] | Room[] {
		const mField = Object.keys(mComp)[Object.values(mComp).length - 1].split("_")[1];
		const threshold = Object.values(mComp)[Object.values(mComp).length - 1];
		if (queryType === "section") {
			entries = entries as Section[];
			if (mField === "avg") {
				return entries.filter((entry) => mCompFunc(entry.avg, threshold));
			} else if (mField === "pass") {
				return entries.filter((entry) => mCompFunc(entry.pass, threshold));
			} else if (mField === "fail") {
				return entries.filter((entry) => mCompFunc(entry.fail, threshold));
			} else if (mField === "audit") {
				return entries.filter((entry) => mCompFunc(entry.audit, threshold));
			} else {
				return entries.filter((entry) => mCompFunc(entry.year, threshold));
			}
		} else {
			entries = entries as Room[];
			if (mField === "lat") {
				return entries.filter((entry) => mCompFunc(entry.lat, threshold));
			} else if (mField === "lon") {
				return entries.filter((entry) => mCompFunc(entry.lon, threshold));
			} else {
				return entries.filter((entry) => mCompFunc(entry.seats, threshold));
			}
		}
	}

	public static performSComparison(
		sComp: object,
		sCompFunc: (x: string, y: string) => boolean,
		entries: Section[] | Room[],
		queryType: string
	): Section[] | Room[] {
		const sField = Object.keys(sComp)[Object.values(sComp).length - 1].split("_")[1];
		const criteria = Object.values(sComp)[Object.values(sComp).length - 1];
		if (queryType === "section") {
			entries = entries as Section[];
			if (sField === "dept") {
				return entries.filter((entry) => sCompFunc(entry.dept, criteria));
			} else if (sField === "id") {
				return entries.filter((entry) => sCompFunc(entry.id, criteria));
			} else if (sField === "instructor") {
				return entries.filter((entry) => sCompFunc(entry.instructor, criteria));
			} else if (sField === "title") {
				return entries.filter((entry) => sCompFunc(entry.title, criteria));
			} else {
				return entries.filter((entry) => sCompFunc(entry.uuid.toString(), criteria));
			}
		} else {
			entries = entries as Room[];
			if (sField === "fullname") {
				return entries.filter((entry) => sCompFunc(entry.fullname, criteria));
			} else if (sField === "shortname") {
				return entries.filter((entry) => sCompFunc(entry.shortname, criteria));
			} else if (sField === "number") {
				return entries.filter((entry) => sCompFunc(entry.number, criteria));
			} else if (sField === "name") {
				return entries.filter((entry) => sCompFunc(entry.name, criteria));
			} else if (sField === "address") {
				return entries.filter((entry) => sCompFunc(entry.address, criteria));
			} else if (sField === "type") {
				return entries.filter((entry) => sCompFunc(entry.type, criteria));
			} else if (sField === "furniture") {
				return entries.filter((entry) => sCompFunc(entry.furniture, criteria));
			} else {
				return entries.filter((entry) => sCompFunc(entry.href, criteria));
			}
		}
	}
}
