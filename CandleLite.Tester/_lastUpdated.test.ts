import crypto from "node:crypto";
import { describe, ExpectStatic, test } from "vitest";
import { fhir } from "./utils";

function randomString(bytes: number) {
    return crypto.randomBytes(bytes).toString("hex");
}

function sortUnique<T>(arr: T[]) {
    return Array.from(new Set(arr)).sort();
}

async function createPatients(
    num: number,
    expect: ExpectStatic,
    familyName: string
) {
    const createdIds = await Promise.all(
        new Array(num).fill(1).map(async (i) => {
            const createResp = await fhir("POST", "/Patient", {
                resourceType: "Patient",
                name: [{ family: familyName, given: [`John-${i}`] }],
            });
            // console.log("Created", createResp.id, createResp.meta?.lastUpdated);
            return createResp.id;
        })
    );

    expect(createdIds.length).toBe(10);
    const uniqueIds = new Set(createdIds);
    expect(uniqueIds.size).toBe(10);
    return createdIds;
}

async function pause(ms: number) {
    return new Promise((resolve) => setTimeout(resolve, ms));
}

describe("_lastUpdated searches", () => {
    test("Search by _lastUpdated", async ({ expect }) => {
        const familyName = randomString(10);
        const ids1 = await createPatients(10, expect, familyName);
        await pause(10);
        const t1 = new Date().toISOString();
        await pause(10);
        // console.log(t1);
        const ids2 = await createPatients(10, expect, familyName);

        // test gt
        const resp1 = await fhir(
            "GET",
            `/Patient?_lastUpdated=gt${t1}&family=${familyName}`
        );
        expect(resp1.resourceType).toBe("Bundle");
        expect(resp1.entry.length).toBe(10);
        expect(sortUnique(resp1.entry.map((e) => e.resource.id))).toEqual(
            ids2.sort()
        );

        // test lt
        const resp2 = await fhir(
            "GET",
            `/Patient?_lastUpdated=lt${t1}&family=${familyName}`
        );
        expect(resp2.resourceType).toBe("Bundle");
        expect(resp2.entry.length).toBe(10);
        expect(sortUnique(resp2.entry.map((e) => e.resource.id))).toEqual(
            ids1.sort()
        );
    });
});
