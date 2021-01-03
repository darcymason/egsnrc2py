from typing import Dict
from typing_extensions import Literal

from jax import jit, random
import jax.numpy as jnp

ParticleKeys = Literal["position", "direction", "energy"]

Particles = Dict[ParticleKeys, jnp.DeviceArray]


def random_walk(rng_key, particles: Particles, iterations):
    num_particles = particles["position"].shape[-1]

    for _ in range(iterations):
        random_normal_numbers = random.normal(rng_key, shape=(7, num_particles))
        (rng_key,) = random.split(rng_key, 1)

        particles["position"] += random_normal_numbers[0:3, :]
        particles["direction"] += random_normal_numbers[3:6, :]
        particles["energy"] += random_normal_numbers[7, :]

    return rng_key, particles


random_walk = jit(random_walk, static_argnums=[2])


def particles_zeros(num_particles):
    num_particles = int(num_particles)

    particles: Particles = {
        "position": jnp.zeros((3, num_particles)),
        "direction": jnp.zeros((3, num_particles)),
        "energy": jnp.zeros((1, num_particles)),
    }

    return particles


def main():
    seed = 0
    rng_key = random.PRNGKey(seed)
    num_particles = 1e6
    iterations = 10

    particles = particles_zeros(num_particles)

    rng_key, particles = random_walk(rng_key, particles, iterations)


if __name__ == "__main__":
    main()
