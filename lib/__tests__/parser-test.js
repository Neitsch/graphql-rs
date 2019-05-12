const graphql_rs = require("../index");
const graphql = require("graphql");

describe("parses graphql", () => {
  it("simple", () => {
    expect(
      graphql_rs.parse("type User { name: String id: ID }")
    ).toMatchSnapshot();
  });
  xit("compare", () => {
    const sourceText = "type User { name: String id: ID }";
    expect(graphql_rs.parse(sourceText)).toEqual(graphql.parse(sourceText));
  });
});
