import {Query} from "../model/Query";
import {Section} from "../model/dataset/Section";
import {Room} from "../model/dataset/Room";
import {InsightResult} from "./IInsightFacade";
import {ValidateQueryStrings} from "./ValidateQueryStrings";
import {ValidateQueryFunctions} from "./ValidateQueryFunctions";

export class PerformQueryOptionsFunctions {
	public static performOPTIONS(
		q: Query,
		filteredEntries: Section[] | Room[] | InsightResult[],
		queryType: string
	): InsightResult[] {
		const cols = this.performCOLUMNS(q, filteredEntries, queryType);
		return this.performORDER(q, cols, queryType);
	}

	public static performCOLUMNS(
		q: Query,
		filteredEntries: Section[] | Room[] | InsightResult[],
		queryType: string
	): InsightResult[] {
		if (q.TRANSFORMATIONS?.APPLY !== undefined) {
			return filteredEntries as InsightResult[];
		}
		let filteredEntriesArray;
		let cols: InsightResult[] = [];
		if (queryType === "section") {
			filteredEntriesArray = filteredEntries as Section[];
		} else {
			filteredEntriesArray = filteredEntries as Room[];
		}
		for (let i = 0; i < filteredEntriesArray.length; i++) {
			cols[i] = {};
			for (const item of q.OPTIONS.COLUMNS) {
				let itemField = item.split("_")[1];
				type EntryKey = keyof InsightResult;
				const key = item as EntryKey;
				cols[i][key] = filteredEntriesArray[i].getProperty(itemField);
			}
		}
		return cols;
	}

	public static performORDER(q: Query, cols: InsightResult[], queryType: string): InsightResult[] {
		if (q.OPTIONS.ORDER === undefined) {
			return cols;
		}
		if (typeof q.OPTIONS.ORDER === "string") {
			return this.performOneKeyORDER(q, cols, queryType);
		} else if (q.OPTIONS.ORDER.dir !== undefined && q.OPTIONS.ORDER.keys !== undefined) {
			return this.performDirAndKeyORDER(q, cols, queryType);
		} else {
			throw new Error("Not a valid ORDER");
		}
	}

	public static performOneKeyORDER(q: Query, cols: InsightResult[], queryType: string): InsightResult[] {
		if (q.OPTIONS.ORDER === undefined || typeof q.OPTIONS.ORDER !== "string") {
			throw new Error("Invalid One Key ORDER");
		}
		if (ValidateQueryStrings.mKey(q.OPTIONS.ORDER, ValidateQueryFunctions.getDatasetId(q), queryType)) {
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

	public static performDirAndKeyORDER(q: Query, cols: InsightResult[], queryType: string): InsightResult[] {
		if (typeof q.OPTIONS.ORDER !== "string" && q.OPTIONS.ORDER !== undefined) {
			type ObjectKey = keyof InsightResult;
			let orderKey = q.OPTIONS.ORDER.keys[0] as ObjectKey;
			let keys = q.OPTIONS.ORDER.keys;
			let stringComparator: (arg0: InsightResult, arg1: InsightResult, arg2: keyof InsightResult) => number;
			let numComparator: (arg0: InsightResult, arg1: InsightResult, arg2: keyof InsightResult) => number;

			if (q.OPTIONS.ORDER?.dir === "UP") {
				stringComparator = (x: InsightResult, y: InsightResult, objKey: keyof InsightResult) => {
					if (x[objKey] > y[objKey]) {
						return 1;
					} else if (x[objKey] < y[objKey]) {
						return -1;
					} else {
						return 0;
					}
				};
				numComparator = (x: InsightResult, y: InsightResult, objKey: keyof InsightResult) => {
					return (x[objKey] as number) - (y[objKey] as number);
				};
			} else {
				stringComparator = (x: InsightResult, y: InsightResult, objKey: keyof InsightResult) => {
					if (x[objKey] > y[objKey]) {
						return -1;
					} else if (x[objKey] < y[objKey]) {
						return 1;
					} else {
						return 0;
					}
				};
				numComparator = (x: InsightResult, y: InsightResult, objKey: keyof InsightResult) => {
					return (y[objKey] as number) - (x[objKey] as number);
				};
			}
			return this.sorting(cols, keys, orderKey, q, stringComparator, numComparator, queryType);
		}
		throw new Error("Invalid SORT");
	}

	private static sorting(
		cols: InsightResult[],
		keys: string[],
		orderKey: string | number,
		q: Query,
		stringComparator: (a0: InsightResult, a1: InsightResult, a2: keyof InsightResult) => number,
		numComparator: (a0: InsightResult, a1: InsightResult, a2: keyof InsightResult) => number,
		queryType: string
	) {
		type ObjectKey = keyof InsightResult;
		return cols.sort((e1, e2) => {
			for (let key in keys) {
				orderKey = key as ObjectKey;
				let result;
				if (ValidateQueryStrings.sKey(key, ValidateQueryFunctions.getDatasetId(q), queryType)) {
					result = stringComparator(e1, e2, orderKey);
				} else {
					result = numComparator(e1, e2, orderKey);
				}
				if (result !== 0) {
					return result;
				}
			}
			return 0;
		});
	}
}
