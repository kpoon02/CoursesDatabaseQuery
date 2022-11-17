import {Query} from "../model/Query";
import {InsightResult, ResultTooLargeError} from "./IInsightFacade";
import {ValidateQueryFunctions} from "./ValidateQueryFunctions";
import {Section} from "../model/dataset/Section";
import Decimal from "decimal.js";
import {ValidateQueryStrings} from "./ValidateQueryStrings";
import {Room} from "../model/dataset/Room";

export class PerformQueryTransformationsFunctions {
	public static performTRANSFORMATIONS(
		q: Query,
		entries: Section[] | Room[],
		queryType: string
	): Section[] | Room[] | InsightResult[] {
		if (q.TRANSFORMATIONS === undefined) {
			return entries;
		}
		const result = PerformQueryTransformationsFunctions.performAPPLYANDCOLUMN(
			q,
			this.performGROUP(q, entries, queryType),
			queryType
		);
		if (result.length > 5000) {
			throw new ResultTooLargeError("Result size is over 5000");
		} else {
			return result;
		}
	}

	public static performGROUP(
		q: Query,
		entries: Section[] | Room[],
		queryType: string
	): Map<string, Section[] | Room[]> {
		if (q.TRANSFORMATIONS?.GROUP === undefined || q.TRANSFORMATIONS.GROUP.length === 0) {
			throw new Error("GROUP not defined or empty");
		}
		const groupCriteria = q.TRANSFORMATIONS?.GROUP;
		let groups = new Map<string, Section[] | Room[]>();
		for (let s of entries) {
			let groupKey: string = "";
			for (let criteria of groupCriteria) {
				groupKey = groupKey + " " + s.getProperty(criteria.split("_")[1]);
			}
			let flag = -1;
			for (let key of groups.keys()) {
				if (groupKey === key) {
					flag = 1;
					if (queryType === "section") {
						let oldValues: Section[] | Room[] | undefined = groups.get(key);
						oldValues = oldValues as Section[];
						s = s as Section;
						if (oldValues !== undefined) {
							oldValues.push(s);
							groups.set(key, oldValues);
						}
					} else {
						let oldValues: Section[] | Room[] | undefined = groups.get(key);
						oldValues = oldValues as Room[];
						s = s as Room;
						if (oldValues !== undefined) {
							oldValues.push(s);
							groups.set(key, oldValues);
						}
					}
				}
			}
			if (flag === -1) {
				if (queryType === "section") {
					s = s as Section;
					groups.set(groupKey, [s]);
				} else {
					s = s as Room;
					groups.set(groupKey, [s]);
				}
			}
		}
		return groups;
	}

	public static performAPPLYANDCOLUMN(
		q: Query,
		groups: Map<string, Section[] | Room[]>,
		queryType: string
	): InsightResult[] {
		let applyRulesResults: Map<string, number[]> = this.performAPPLY(q, groups);
		let newEntries: InsightResult[] = [];
		let i = 0;
		for (let key of groups.keys()) {
			newEntries[i] = {};
			for (let item of q.OPTIONS.COLUMNS) {
				type EntryKey = keyof InsightResult;
				const colKey = item as EntryKey;
				if (
					ValidateQueryStrings.mKey(item, ValidateQueryFunctions.getDatasetId(q), queryType) ||
					ValidateQueryStrings.sKey(item, ValidateQueryFunctions.getDatasetId(q), queryType)
				) {
					let itemField = item.split("_")[1];
					let mappedValues = groups.get(key);
					mappedValues = mappedValues as Section[];
					newEntries[i][colKey] = mappedValues[0].getProperty(itemField);
				} else {
					let applyResult = applyRulesResults.get(item) as number[];
					newEntries[i][colKey] = applyResult[i];
				}
			}
			i++;
		}
		return newEntries;
	}

	private static performAPPLY(q: Query, groups: Map<string, Section[] | Room[]>): Map<string, number[]> {
		if (q.TRANSFORMATIONS?.APPLY === undefined) {
			return new Map<string, number[]>();
		}
		let result: Map<string, number[]> = new Map<string, number[]>();
		for (let applyRule of q.TRANSFORMATIONS.APPLY) {
			for (let key of groups.keys()) {
				let applyKey = Object.keys(applyRule)[0];
				let applyToken = Object.keys(Object.values(applyRule)[0])[0];
				let keyValue = Object.values(Object.values(applyRule)[0])[0];
				let itemField = keyValue.split("_")[1];
				if (applyToken === "MAX") {
					this.performMAX(groups, key, itemField, result, applyKey);
				} else if (applyToken === "MIN") {
					this.performMIN(groups, key, itemField, result, applyKey);
				} else if (applyToken === "AVG") {
					this.performAVG(groups, key, itemField, result, applyKey);
				} else if (applyToken === "COUNT") {
					this.performCOUNT(groups, key, itemField, result, applyKey);
				} else if (applyToken === "SUM") {
					this.performSUM(groups, key, itemField, result, applyKey);
				}
			}
		}
		return result;
	}

	private static performSUM(
		groups: Map<string, Section[] | Room[]>,
		key: any,
		itemField: string,
		result: Map<string, number[]>,
		applyKey: string
	) {
		let total = new Decimal(0);
		for (let entry of groups.get(key) as Section[] | Room[]) {
			let entryNum = new Decimal(entry.getProperty(itemField) as number);
			total = total.add(entryNum);
		}
		let res = Number(total.toFixed(2));
		if (!result.has(applyKey)) {
			result.set(applyKey, [res]);
		} else {
			let oldValues: number[] | undefined = result.get(applyKey);
			if (oldValues !== undefined) {
				result.set(applyKey, oldValues.concat([res]));
			}
		}
	}

	private static performCOUNT(
		groups: Map<string, Section[] | Room[]>,
		key: any,
		itemField: string,
		result: Map<string, number[]>,
		applyKey: string
	) {
		let uniqueValues = new Set<string | number>();
		for (let entry of groups.get(key) as Section[] | Room[]) {
			let value = entry.getProperty(itemField);
			uniqueValues.add(value);
		}
		let count = uniqueValues.size;
		if (!result.has(applyKey)) {
			result.set(applyKey, [count]);
		} else {
			let oldValues: number[] | undefined = result.get(applyKey);
			if (oldValues !== undefined) {
				result.set(applyKey, oldValues.concat([count]));
			}
		}
	}

	private static performAVG(
		groups: Map<string, Section[] | Room[]>,
		key: any,
		itemField: string,
		result: Map<string, number[]>,
		applyKey: string
	) {
		let total = new Decimal(0);
		for (let entry of groups.get(key) as Section[] | Room[]) {
			let entryNum = new Decimal(entry.getProperty(itemField) as number);
			total = total.add(entryNum);
		}
		let entry: Section[] | Room[] = groups.get(key) as Section[] | Room[];
		let numRows = entry.length;
		let avg = total.toNumber() / numRows;
		let res = Number(avg.toFixed(2));
		if (!result.has(applyKey)) {
			result.set(applyKey, [res]);
		} else {
			let oldValues: number[] | undefined = result.get(applyKey);
			if (oldValues !== undefined) {
				result.set(applyKey, oldValues.concat([res]));
			}
		}
	}

	private static performMIN(
		groups: Map<string, Section[] | Room[]>,
		key: any,
		itemField: string,
		result: Map<string, number[]>,
		applyKey: string
	) {
		let min = Infinity;
		for (let entry of groups.get(key) as Section[] | Room[]) {
			if (entry.getProperty(itemField) < min) {
				min = entry.getProperty(itemField) as number;
			}
		}
		if (!result.has(applyKey)) {
			result.set(applyKey, [min]);
		} else {
			let oldValues: number[] | undefined = result.get(applyKey);
			if (oldValues !== undefined) {
				result.set(applyKey, oldValues.concat([min]));
			}
		}
	}

	private static performMAX(
		groups: Map<string, Section[] | Room[]>,
		key: any,
		itemField: string,
		result: Map<string, number[]>,
		applyKey: string
	) {
		let max = -Infinity;
		for (let entry of groups.get(key) as Section[] | Room[]) {
			if (entry.getProperty(itemField) > max) {
				max = entry.getProperty(itemField) as number;
			}
		}
		if (!result.has(applyKey)) {
			result.set(applyKey, [max]);
		} else {
			let oldValues: number[] | undefined = result.get(applyKey);
			if (oldValues !== undefined) {
				result.set(applyKey, oldValues.concat([max]));
			}
		}
	}
}
