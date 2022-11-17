import {InsightError} from "./IInsightFacade";

export class ValidateQueryStrings {
	public static mKey(key: string, idString: string, queryType: string): boolean {
		if (queryType === "section") {
			return (
				key === idString.concat("_avg") ||
				key === idString.concat("_pass") ||
				key === idString.concat("_fail") ||
				key === idString.concat("_audit") ||
				key === idString.concat("_year")
			);
		} else if (queryType === "room") {
			return (
				key === idString.concat("_lat") || key === idString.concat("_lon") || key === idString.concat("_seats")
			);
		}
		throw new InsightError("Cannot validate mKey");
	}

	public static sKey(key: string, idString: string, queryType: string): boolean {
		if (queryType === "section") {
			return (
				key === idString.concat("_dept") ||
				key === idString.concat("_id") ||
				key === idString.concat("_instructor") ||
				key === idString.concat("_title") ||
				key === idString.concat("_uuid")
			);
		} else if (queryType === "room") {
			return (
				key === idString.concat("_fullname") ||
				key === idString.concat("_shortname") ||
				key === idString.concat("_number") ||
				key === idString.concat("_name") ||
				key === idString.concat("_address") ||
				key === idString.concat("_type") ||
				key === idString.concat("_furniture") ||
				key === idString.concat("_href")
			);
		}
		throw new InsightError("Cannot validate sKey");
	}

	public static validateIdStringOrApplyKey(s: string): boolean {
		if (s.length >= 1 && !s.includes("_")) {
			return true;
		}
		return false;
	}

	public static validateApplyToken(s: string): boolean {
		if (s === "MAX" || s === "MIN" || s === "AVG" || s === "COUNT" || s === "SUM") {
			return true;
		}
		return false;
	}
}
