#[macro_use]
extern crate criterion;

use criterion::black_box;
use criterion::Criterion;

use graphql_rs_native::language::parser::parse;
use graphql_rs_native::language::source::Source;

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("parse big schema", |b| {
        let source = Source::new(
            std::iter::repeat(
                "
            type Author {
                id: Int!
                firstName: String
                lastName: String
                posts: [Post]
            }

            type Post {
                id: Int!
                title: String
                author: Author
                votes: Int
            }

            # the schema allows the following query:
            type Query {
                posts: [Post]
                author(id: Int!): Author
            }

            # this schema allows the following mutation:
            type Mutation {
                upvotePost (
                postId: Int!
                ): Post
            }
            ",
            )
            .take(10)
            .collect::<String>(),
            None,
            None,
        );
        b.iter(|| parse(black_box(&source)))
    });

    c.bench_function("parse Github schema", |b| {
        let source = Source::new(
            std::fs::read_to_string("./benches/github-schema.graphql")
                .expect("Github schema file should be present"),
            None,
            None,
        );
        b.iter(|| parse(black_box(&source)))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
