"""Timings on Google's Colaboratory:

https://colab.research.google.com/drive/1QZKnByThJrq9NhgBbFb8Nx6cPybFVWZJ#scrollTo=am-XsJDDrBU8&line=4&uniqifier=1

CPU:
    random_walk duration: 5368.768 ms
    random_walk duration: 1387.357 ms
    random_walk duration: 1339.349 ms
    random_walk duration: 1359.297 ms
    random_walk duration: 1344.687 ms
    random_walk duration: 1326.127 ms
    random_walk duration: 1358.951 ms
    random_walk duration: 1382.427 ms
    random_walk duration: 1355.318 ms
    random_walk duration: 1377.143 ms

GPU:
    random_walk duration: 1373.592 ms
    random_walk duration: 15.000 ms
    random_walk duration: 14.722 ms
    random_walk duration: 14.642 ms
    random_walk duration: 21.203 ms
    random_walk duration: 14.714 ms
    random_walk duration: 14.782 ms
    random_walk duration: 14.749 ms
    random_walk duration: 14.655 ms
    random_walk duration: 14.642 ms
"""


import time
from typing import Dict, Tuple
from typing_extensions import Literal

import matplotlib.pyplot as plt

from jax import jit, random
import jax.numpy as jnp

ParticleKeys = Literal["position", "direction", "energy"]
Particles = Dict[ParticleKeys, jnp.DeviceArray]


def random_walk(
    prng_key: jnp.DeviceArray, particles: Particles, iterations: int,
) -> Tuple[jnp.DeviceArray, Particles]:
    num_particles = particles["position"].shape[-1]

    for _ in range(iterations):
        random_normal_numbers = random.normal(prng_key, shape=(7, num_particles))
        (prng_key,) = random.split(prng_key, 1)

        particles["position"] += random_normal_numbers[0:3, :]
        particles["direction"] += random_normal_numbers[3:6, :]
        particles["energy"] += random_normal_numbers[7, :]

    return prng_key, particles


random_walk = jit(random_walk, static_argnums=(2,))


def timer(func):
    def wrap(*args, **kwargs):
        start = time.time()
        ret = func(*args, **kwargs)

        # See https://jax.readthedocs.io/en/latest/async_dispatch.html
        # for why this is needed.
        _, particles = ret
        for _, item in particles.items():
            item.block_until_ready()

        stop = time.time()
        duration = (stop - start) * 1000.0
        print("{:s} duration: {:.3f} ms".format(func.__name__, duration))
        return ret

    return wrap


random_walk = timer(random_walk)


def particles_zeros(num_particles: int) -> Particles:
    particles: Particles = {
        "position": jnp.zeros((3, num_particles)),
        "direction": jnp.zeros((3, num_particles)),
        "energy": jnp.zeros((1, num_particles)),
    }

    return particles


def main():
    seed = 0
    prng_key = random.PRNGKey(seed)
    num_particles = int(1e6)
    iterations = 10
    runs = 10

    particles = particles_zeros(num_particles)

    for _ in range(runs):
        prng_key, particles = random_walk(prng_key, particles, iterations)

    plt.scatter(particles["position"][0, 0:1000], particles["position"][1, 0:1000])
    plt.show()


if __name__ == "__main__":
    main()
